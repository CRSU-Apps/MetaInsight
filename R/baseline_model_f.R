#' @title baseline_model
#' @description Fit a baseline risk regression model with `bnma::network.run()`
#' @param regressor_type character. Type of regression coefficient, either `shared`, `unrelated`, or `exchangeable`
#' @param seed x
#' @inheritParams common_params
#' @return List of bnma related output:
#'  mtcResults = model object itself carried through (needed to match existing code).
#'  covariate_value = The mean covariate value, used for centring.
#'  reference_name = The name of the reference treatment.
#'  comparator_names = Vector containing the names of the comparators.
#'  a = text output stating whether fixed or random effects.
#'  cov_value_sentence = text output stating the value for which the covariate has been set to for producing output.
#'  slopes = named list of slopes for the regression equations (unstandardised - equal to one 'increment').
#'  intercepts = named list of intercepts for the regression equations at cov_value.
#'  outcome = The outcome type for the analysis eg. "MD" or "OR".
#'  model = effects type, "fixed" or "random".
#'  covariate_min = Vector of minimum covariate values directly contributing to the regression.
#'  covariate_max = Vector of maximum covariate values directly contributing to the regression.
#'  dic = Summary of model fit
#' @export
baseline_model <- function(connected_data, treatment_df, outcome, reference_treatment, model_type, regressor_type, seed, async = FALSE){

  model <- BaselineRiskRegression(connected_data, treatment_df, outcome, reference_treatment, model_type, cregressor_type, seed)

  output <- BaselineRiskModelOutput(connected_data, treatment_df, model, outcome_measure)

  class(output) <- "baseline_model"

  return(output)
}

#' Takes MetaInsight data and converts it into BNMA format
#'
#' @param connected_data A data frame of data in MetaInsight format
#' @param treatment_df Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome "Continuous" or "Binary"
#' @param reference_treatment An element of treatment_df$Label, the reference treatment.
#' @return List:
#'  - 'ArmLevel' = Data frame containing 'Study', 'Treat', 'N', 'Outcomes', and (for outcome_type="Continuous") 'SD'
#'  - 'Treat.order' = Vector of (unique) treatments, with the reference treatment first
FormatForBnma <- function(connected_data, treatment_df, outcome, reference_treatment) {
  if (FindDataShape(connected_data) == "wide") {
    br_data <- as.data.frame(WideToLong(connected_data, outcome = outcome))
  } else if (FindDataShape(connected_data) == "long") {
    br_data <- connected_data
  } else {
    stop("data_format has to be 'wide' or 'long'")
  }

  #Use wrangled treatment names
  br_data$Treat <- treatment_df$Label[match(connected_data$T, treatment_df$Number)]

  #Treatment order (put reference_treatment first)
  Treat.order <- VectorWithItemFirst(vector = unique(br_data$Treat), first_item = reference_treatment)

  #Arm-level data
  if (outcome == "Binary") {
    ArmLevel <- dplyr::rename(br_data, "Outcomes" = "R")
    ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "N"))
  } else if (outcome == "Continuous") {
    ArmLevel <- dplyr::rename(br_data, "Outcomes" = "Mean")
    ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "SD", "N"))
  } else {
    stop("outcome_type has to be 'Continuous' or 'Binary'")
  }

  return(list(ArmLevel = ArmLevel, Treat.order = Treat.order))
}



#' Creates the baseline risk meta-regression network in BNMA
#'
#' @param br_data A list of data in the format produced by FormatForBnma().
#' @param outcome "Continuous" or "Binary".
#' @param model_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @return Output from bnma::network.data().
BaselineRiskNetwork <- function(br_data, outcome, model_type, cov_parameters) {
  #Use bnma terms
  if (cov_parameters == "shared") {
    cov_parameters <- "common"
  } else if (cov_parameters == "unrelated") {
    cov_parameters <- "independent"
  } else if (cov_parameters != "exchangeable") {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }

  if(outcome == "Binary") {
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                N = ArmLevel$N,
                                                response = "binomial",
                                                Treat.order = Treat.order,
                                                type = model_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  if(outcome == "Continuous") {
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                response = "normal",
                                                SE = ArmLevel$SD / sqrt(ArmLevel$N),
                                                Treat.order = Treat.order,
                                                type = model_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  return(network)
}



#' Fits the baseline risk meta-regression model in BNMA
#'
#' @param connected_data A data frame of data in MetaInsight format.
#' @param treatment_df Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome "Continuous" or "Binary".
#' @param reference treatment An element of treatment_df$Label, the .
#' @param model_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @param seed Seed. Defaults to 123.
#' @return Output from bnma::network.run().
BaselineRiskRegression <- function(connected_data, treatment_df, outcome, reference_treatment, model_type, cov_parameters, seed = 123) {
  formatted_data <- FormatForBnma(connected_data = connected_data, treatment_df = treatment_df,
                                  outcome = outcome, reference_treatment = reference_treatment)
  network <- BaselineRiskNetwork(br_data = formatted_data, outcome = outcome,
                                 model_type = model_type, cov_parameters = cov_parameters)
  #Select random seeds for the four chains based on 'seed'
  set.seed(seed)
  seeds <- sample.int(4, n = .Machine$integer.max)

  #Put the seeds in the required format for passing to bnma::network.run()
  rng_inits <- list()
  for (i in 1:length(seeds)) {
    rng_inits[[i]] <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[i])
  }

  return(bnma::network.run(network,
                           n.run = 20000,
                           max.run = 60000,
                           conv.limit = 1.5,
                           RNG.inits = rng_inits,
                           n.chains = length(seeds)))
}



#' Function to collate model output to be used in the novel graph.
#'
#' @param connected_data Data frame from which model was calculated.
#' @param treatment_df Data frame containing treatment IDs (Number) and names (Label).
#' @param model Completed model object after running BaselineRiskRegression().
#' @param outcome_measure The outcome measure for the analysis: One of: "OR", "RR", "MD".
#' @return List of bnma related output:
#'  mtcResults = model object itself carried through (needed to match existing code).
#'  covariate_value = The mean covariate value, used for centring.
#'  reference_name = The name of the reference treatment.
#'  comparator_names = Vector containing the names of the comparators.
#'  a = text output stating whether fixed or random effects.
#'  cov_value_sentence = text output stating the value for which the covariate has been set to for producing output.
#'  slopes = named list of slopes for the regression equations (unstandardised - equal to one 'increment').
#'  intercepts = named list of intercepts for the regression equations at cov_value.
#'  outcome = The outcome type for the analysis eg. "MD" or "OR".
#'  model = effects type, "fixed" or "random".
#'  covariate_min = Vector of minimum covariate values directly contributing to the regression.
#'  covariate_max = Vector of maximum covariate values directly contributing to the regression.
#'  dic = Summary of model fit
BaselineRiskModelOutput <- function(connected_data, treatment_df, model, outcome_measure) {

  treatments <- model$network$Treat.order
  reference_treatment <- unname(treatments[1])
  comparator_names <- unname(treatments[-1])
  model_summary <- summary(model)

  connected_data$Treatment <- treatment_df$Label[match(connected_data$T, treatment_df$Number)]

  #Create text for random/fixed effect
  model_text <- paste(model$network$type, "effect", sep = " ")

  mean_covariate_value <- model$network$mx_bl

  #Summary sentence of where covariate value has been set for results
  cov_value_sentence <- paste0("Value for baseline risk set at ", mean_covariate_value)

  #Obtain slope(s), which are named b_bl[number]
  slope_indices <- grep("^b_bl\\[[0-9]+\\]$", rownames(model_summary$summary.samples$quantiles))
  slopes <- model_summary$summary.samples$quantiles[slope_indices, "50%"]
  names(slopes) <- treatments

  #Obtain intercept(s), which are named d[number]
  intercept_indices <- grep("^d\\[[0-9]+\\]$", rownames(model_summary$summary.samples$quantiles))
  intercepts_centred <- model_summary$summary.samples$quantiles[intercept_indices, "50%"]
  intercepts <- intercepts_centred - slopes * mean_covariate_value
  names(intercepts) <- treatments

  #Drop the reference treatment, which always has a slope and intercept of 0
  slopes <- slopes[-1]
  intercepts <- intercepts[-1]

  #Convert bnma outcome type into MetaInsight language
  if (model$network$response == "binomial") {
    outcome <- "Binary"
  } else if (model$network$response == "normal") {
    outcome <- "Continuous"
  }

  min_max <- FindCovariateRanges(
    connected_data = connected_data,
    treatment_df = treatment_df,
    reference_treatment = reference_treatment,
    covariate_title = NULL,
    baseline_risk = TRUE,
    outcome = outcome,
    model = model
  )

  dic <- BaselineRiskDicTable(model)

  return(list(mtcResults = model,
              covariate_value = mean_covariate_value,
              reference_name = reference_treatment,
              comparator_names = comparator_names,
              a = model_text,
              cov_value_sentence = cov_value_sentence,
              slopes = slopes,
              intercepts = intercepts,
              outcome = outcome,
              model = model$network$type,
              covariate_min = min_max$min,
              covariate_max = min_max$max,
              dic = dic
  )
  )
}



#' An equivalent to {gemtc}'s relative.effect() function, for baseline risk in {bnma}.
#'
#' @param model bnma model object created by BaselineRiskRegression().
#' @param covariate_value The covariate value at which to calculate relative effects.
#' @return Matrix with the median and 95\% credible interval relative effect
#'  - columns: '50%', '2.5%' and '97.5%'
#'  - rows: one row per non-reference treatment, named by the corresponding treatment parameter (e.g. the first one is `d[2]`).
BnmaRelativeEffects <- function(model, covariate_value) {
  model_summary <- summary(model)
  parameters <- rownames(model_summary$summary.samples$quantiles)
  #Extract parameters that begin with "d[", except d[1]
  treatment_parameters <- grep("d\\[([0-9][0-9]+|[2-9])\\]",
                               parameters,
                               value = TRUE)
  #Extract parameters that begin with "b_bl[", except b_bl[1]
  covariate_parameters <- grep("b_bl\\[([0-9][0-9]+|[2-9])\\]",
                               parameters,
                               value = TRUE)
  centred_covariate_value <- covariate_value - model$network$mx_bl

  samples <- list()
  #The number of chains is always left at the default 3
  for (chain in 1:3) {
    samples[[chain]] <- model$samples[[chain]][, treatment_parameters] + centred_covariate_value * model$samples[[chain]][, covariate_parameters]
  }

  relative_effects <- MCMCvis::MCMCsummary(samples)
  return(as.matrix(relative_effects[, c("50%", "2.5%", "97.5%")]))
}



#' Calculate the confidence regions within direct evidence for the baseline risk model.
#'
#' @param model_output Return from `BaselineRiskModelOutput()`.
#'
#' @return list of confidence region objects and confidence interval objects.
#' Regions cover treatments with a non-zero covariate range of direct contributions,
#' intervals cover treatments with a single covariate value from direct contributions.
#' Any treatment with no direct contributions will not be present in either list.
#' Each is a list of data frames for each treatment name. Each data frame contains 3 columns:
#' - cov_value: The covariate value at which the confidence region is calculated.
#' - lower: the 2.5% quantile.
#' - upper: the 97.5% quantile.
#' Each data frame in "regions" contains 11 rows creating a 10-polygon region.
#' Each data frame in "intervals" contains a single row at the covariate value of that single contribution.
CalculateConfidenceRegionsBnma <- function(model_output) {

  mtc_results <- model_output$mtcResults
  treatments <- mtc_results$network$Treat.order

  confidence_regions <- list()
  confidence_intervals <- list()

  for (treatment_name in model_output$comparator_names) {
    parameter_name <- glue::glue("d[{which(treatment_name == unname(treatments))}]")
    cov_min <- model_output$covariate_min[treatment_name]
    cov_max <- model_output$covariate_max[treatment_name]

    if (is.na(cov_min)) {
      confidence_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else if (cov_min == cov_max) {
      interval <- .FindConfidenceIntervalBnma(mtc_results, cov_min, parameter_name)
      df <- data.frame(cov_value = cov_min, lower = interval["2.5%"], upper = interval["97.5%"])

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      confidence_intervals[[treatment_name]] <- df
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else {
      df <- data.frame()
      for (cov_value in seq(from = cov_min, to = cov_max, length.out = 11)) {
        interval <- .FindConfidenceIntervalBnma(mtc_results, cov_value, parameter_name)
        df <- rbind(
          df,
          data.frame(cov_value = cov_value, lower = interval["2.5%"], upper = interval["97.5%"])
        )
      }

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      confidence_regions[[treatment_name]] <- df
      confidence_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    }
  }

  return(
    list(
      regions = confidence_regions,
      intervals = confidence_intervals
    )
  )
}



#' Find the confidence interval at a given covariate value.
#'
#' @param mtc_results Meta-analysis object from which to find confidence interval.
#' @param reference_name Name of reference treatment.
#' @param cov_value Covariate value at which to find the confidence interval.
#' @param parameter_name Name of the parameter for which to get the confidence interval.
#'
#' @return Named vector of "2.5%" and "97.5" quantiles.
.FindConfidenceIntervalBnma <- function(mtc_results, cov_value, parameter_name) {
  rel_eff <- BnmaRelativeEffects(model = mtc_results, covariate_value = cov_value)
  return(rel_eff[parameter_name, c("2.5%", "97.5%")])
}



#' Creates a DIC table in gemtc format from a bnma model
#'
#' @param br_model Output from bnma::network.run(), typically created from BaselineRiskRegression().
#' @return A DIC table in the same format as from gemtc.
BaselineRiskDicTable <- function(br_model) {
  summary <- summary(br_model)
  dic_table <- c(summary$deviance, summary$total_n)
  names(dic_table)[4] <- "Data points"
  return(dic_table)
}



#' Puts a relative effects table from bnma into gemtc format
#'
#' @param median_ci_table Output from bnma::relative.effects.table(, summary_stat = "ci")
#' @return A relative effects table in the same format as from gemtc.
BaselineRiskRelativeEffectsTable <- function(median_ci_table) {
  #Entries in the input table are in the form "[lower_ci,median,upper_ci]" (no spaces)

  #The dimensions of the (square) table
  dim_median <- nrow(median_ci_table)
  #Create matrices to store the lower_ci, median and upper_ci separately
  lower_ci <- matrix(nrow = dim_median, ncol = dim_median)
  median_br <- matrix(nrow = dim_median, ncol = dim_median)
  upper_ci <- matrix(nrow = dim_median, ncol = dim_median)

  for (row in 1:dim_median) {
    for (col in 1:dim_median) {
      #Extract lower_ci, median and upper_ci
      interval <- round(
        as.numeric(
          stringr::str_extract_all(string = median_ci_table[row, col],
                                   pattern = "[-0-9\\.]+")[[1]]
        ), digits = 2
      )
      lower_ci[row, col] <- interval[1]
      median_br[row, col] <- interval[2]
      upper_ci[row, col] <- interval[3]
    }
  }

  #Paste into the format "median (lower_ci, upper_ci)"
  median_ci_table_new <- matrix(paste0(median_br, " (", lower_ci, ", ", upper_ci, ")"), nrow = dim_median)
  diag(median_ci_table_new) <- rownames(median_ci_table)
  rownames(median_ci_table_new) <- rownames(median_ci_table)
  colnames(median_ci_table_new) <- colnames(median_ci_table)

  return(median_ci_table_new)
}



#' Change the ranking direction in a bnma ranking table.
#'
#' @param ranking_table The $rank.tx table from output from bnma::network.run()
#' @return A relative effects table in the same format as from gemtc.
BnmaSwitchRanking <- function(ranking_table) {
  ranking_table <- cbind(ranking_table, data.frame(new_ranks = nrow(ranking_table):1))
  new_table <- dplyr::arrange(ranking_table, ranking_table$new_ranks)
  rownames(new_table) <- rownames(ranking_table)
  return(as.matrix(dplyr::select(new_table, !"new_ranks")))
}



#' Get the parameters that are to be displayed in Gelman plots.
#'
#' @param all_parameters Vector of monitored parameters from a bnma model.
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @return Vector of treatment effect and covariate parameter names, plus random effects sd and/or exchangeable covariate sd.
GetBnmaParameters <- function(all_parameters, effects_type, cov_parameters) {
  #Extract parameters which begin with "d[" or "b_bl[", except d[1] and b_bl[1]
  parameters <- grep("(d|b_bl)\\[([0-9][0-9]+|[2-9])\\]",
                     all_parameters,
                     value = TRUE)
  if (effects_type == "random") {
    parameters <- c(parameters, "sd")
  } else if (effects_type != "fixed") {
    stop("effects_type must be 'fixed' or 'random'")
  }
  if (cov_parameters == "exchangeable") {
    parameters <- c(parameters, "sdB")
  } else if (!cov_parameters %in% c("shared", "unrelated")) {
    stop("cov_parameters must be 'shared', 'exchangeable' or 'unrelated'")
  }
  return(parameters)
}




#' An equivalent to {gemtc}'s relative.effect() function, for baseline risk in {bnma}.
#'
#' @param model bnma model object created by BaselineRiskRegression().
#' @param covariate_value The covariate value at which to calculate relative effects.
#' @return Matrix with the median and 95% credible interval relative effect
#'  - columns: '50%', '2.5%' and '97.5%'
#'  - rows: one row per non-reference treatment, named by the corresponding treatment parameter (e.g. the first one is `d[2]`).
BnmaRelativeEffects <- function(model, covariate_value) {

  parameters <- colnames(model$samples[[1]])
  #Extract parameters that begin with "d[", except d[1]
  treatment_parameters <- grep("d\\[([0-9][0-9]+|[2-9])\\]",
                               parameters,
                               value = TRUE)
  #Extract parameters that begin with "b_bl[", except b_bl[1]
  covariate_parameters <- grep("b_bl\\[([0-9][0-9]+|[2-9])\\]",
                               parameters,
                               value = TRUE)
  centred_covariate_value <- covariate_value - model$network$mx_bl

  samples <- list()
  #The number of chains is always left at the default 3
  for (chain in 1:3) {
    samples[[chain]] <- model$samples[[chain]][, treatment_parameters] + centred_covariate_value * model$samples[[chain]][, covariate_parameters]
  }

  relative_effects <- MCMCvis::MCMCsummary(samples)
  return(as.matrix(relative_effects[, c("50%", "2.5%", "97.5%")]))
}


BaselineRiskDicTable <- function(br_model) {
  summary <- summary(br_model)
  dic_table <- c(summary$deviance, summary$total_n)
  names(dic_table)[4] <- "Data points"
  return(dic_table)
}
