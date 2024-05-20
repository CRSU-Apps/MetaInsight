#' Takes MetaInsight data and converts it into BNMA format
#'
#' @param br_data A data frame of data in MetaInsight format
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome_type "Continuous" or "Binary"
#' @param ref An element of treatment_ids$Label, the reference treatment.
#' @return List:
#'  - 'ArmLevel' = Data frame containing 'Study', 'Treat', 'N', 'Outcomes', and (for outcome_type="Continuous") 'SD'
#'  - 'Treat.order' = Vector of (unique) treatments, with the reference treatment first
FormatForBnma <- function(br_data, treatment_ids, outcome_type, ref) {
  if (FindDataShape(br_data) == "wide") {
    br_data2 <- as.data.frame(WideToLong(br_data, outcome_type=outcome_type))
  } else if (FindDataShape(br_data) == "long") {
    br_data2 <- br_data
  } else {
    stop("data_format has to be 'wide' or 'long'")
  }

  #Use wrangled treatment names
  br_data3 <- br_data2
  br_data3$Treat <- treatment_ids$Label[match(br_data2$T, treatment_ids$Number)]
  
  #Treatment order (put 'ref' first)
  Treat.order <- VectorWithItemFirst(vector = unique(br_data3$Treat), first_item = ref)
  
  #Arm-level data
   if (outcome_type == "Binary") {
     ArmLevel <- dplyr::rename(br_data3, "Outcomes" = "R")
     ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "N"))
   } else if (outcome_type == "Continuous") {
     ArmLevel <- dplyr::rename(br_data3, "Outcomes" = "Mean")
     ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "SD", "N"))
   } else {
     stop("outcome_type has to be 'Continuous' or 'Binary'")
   }
  
   return(list(ArmLevel = ArmLevel, Treat.order = Treat.order))
}



#' Creates the baseline risk meta-regression network in BNMA
#'
#' @param br_data A list of data in the format produced by FormatForBnma().
#' @param outcome_type "Continuous" or "Binary".
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @return Output from bnma::network.data().
BaselineRiskNetwork <- function(br_data, outcome_type, effects_type, cov_parameters) {
  #Use bnma terms
  if (cov_parameters == "shared") {
    cov_parameters <- "common"
  } else if (cov_parameters == "unrelated") {
    cov_parameters <- "independent"
  } else if (cov_parameters != "exchangeable") {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }
  
  if(outcome_type == "Binary") {
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                N = ArmLevel$N,
                                                response = "binomial",
                                                Treat.order = Treat.order,
                                                type = effects_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  if(outcome_type == "Continuous") {
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                response = "normal",
                                                SE = ArmLevel$SD / sqrt(ArmLevel$N),
                                                Treat.order = Treat.order,
                                                type = effects_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  return(network)
}



#' Fits the baseline risk meta-regression model in BNMA
#'
#' @param br_data A data frame of data in MetaInsight format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param ref An element of treatment_ids$Label, the reference treatment.
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @param seed Seed. Defaults to 123.
#' @return Output from bnma::network.run().
BaselineRiskRegression <- function(br_data, treatment_ids, outcome_type, ref,  effects_type, cov_parameters, seed = 123) {
  formatted_data <- FormatForBnma(br_data = br_data, treatment_ids = treatment_ids,
                                  outcome_type = outcome_type, ref = ref)
  network <- BaselineRiskNetwork(br_data = formatted_data, outcome_type = outcome_type,
                                 effects_type = effects_type, cov_parameters = cov_parameters)
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
#' @param data Data frame from which model was calculated.
#' @param treatment_ids Data frame containing treatment IDs (Number) and names (Label).
#' @param model Completed model object after running BaselineRiskRegression().
#' @param outcome_measure The outcome measure for the analysis: One of: "OR", "RR", "MD".
#' @return List of bnma related output:
#'  mtcResults = model object itself carried through (needed to match existing code).
#'  covariate_value = The covariate value originally passed into this function.
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
BaselineRiskModelOutput <- function(data, treatment_ids, model, outcome_measure) {
  
  treatments <- model$network$Treat.order
  reference <- unname(treatments[1])
  comparator_names <- unname(treatments[-1])
  model_summary <- summary(model)
  
  data$Treatment <- treatment_ids$Label[match(data$T, treatment_ids$Number)]
  
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
  intercepts <- model_summary$summary.samples$quantiles[intercept_indices, "50%"]
  names(intercepts) <- treatments
  
  #Drop the reference treatment, which always has a slope and intercept of 0
  slopes <- slopes[-1]
  intercepts <- intercepts[-1]
  
  #Convert bnma outcome type into MetaInsight language
  if (model$network$response == "binomial") {
    outcome_type <- "Binary"
  } else if (model$network$response == "normal") {
    outcome_type <- "Continuous"
  }
  
  min_max <- FindCovariateRanges(
    data = data,
    treatment_ids = treatment_ids,
    reference = reference,
    covariate_title = NULL,
    baseline_risk = TRUE,
    outcome_type = outcome_type,
    model = model
  )
  
  return(list(mtcResults = model,
              covariate_value = mean_covariate_value,
              reference_name = reference,
              comparator_names = comparator_names,
              a = model_text,
              cov_value_sentence = cov_value_sentence,
              slopes = slopes,
              intercepts = intercepts,
              outcome = outcome_measure,
              model = model$network$type,
              covariate_min = min_max$min,
              covariate_max = min_max$max
              )
  )
}



#' Get the outcome in the reference arm.
#' 
#' @param data Data in long format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Binary" or "Continuous".
#' @param observed "Observed" or "Imputed". See @return.
#' @param model Model created by bnma::network.run(). Only required when @param observed == "Imputed". Defaults to NULL.
#' 
#' @return Vector of reference arm outcomes, named by study.
#'   If a study contains the reference treatment then the value returned is the observed outcome in the reference arm.
#'   If a study does not contain the reference treatment then the value returned is:
#'     - NA if @param observed == "Observed";
#'     - The median of the study-specific intercept parameter if @param observed == "Imputed".
GetReferenceOutcome <- function(data, treatment_ids, outcome_type, observed, model = NULL){
  
  if (!(observed %in% c("Observed", "Imputed"))) {
    stop("'observed' must be 'Observed' or 'Imputed'")
  }
  
  if (observed == "Imputed" && is.null(model)) {
    stop("A model must be provided when 'observed' == 'Imputed'") 
  }
  
  treatments <- treatment_ids$Label
  data$Treatment <- treatments[match(data$T, treatment_ids$Number)]
  
  #Data with only control treatment rows kept
  data_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "keep")
  if (outcome_type == "Binary"){
    #Add NAs as required by metafor::escalc()
    data_control$R[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "PLO",
                                    xi = data_control$R,
                                    ni = data_control$N)
  } else if (outcome_type == "Continuous"){
    #Add NAs as required by metafor::escalc()
    data_control$Mean[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "MN",
                                    mi = data_control$Mean,
                                    sdi = data_control$SD,
                                    ni = data_control$N)
  } else{
    stop("'outcome_type' must be 'Continuous' or 'Binary'")
  }
  outcomes <- as.numeric(effect_sizes$yi)
  names(outcomes) <- unique(data_control$Study)
  
  #If imputed values are requested and there are any missing outcomes then use the imputed outcomes
  if (observed == "Imputed" && any(is.na(outcomes))) {
    #Eta is the study-specific intercept parameter, which is often called mu in the literature. When a study does not contain the reference arm, {bnma} adds the reference arm to the data with missing values. Eta is then imputed for that study.
    imputed_outcomes <- MCMCvis::MCMCsummary(object = model$samples, params = "Eta")["50%"][[1]]
    names(imputed_outcomes) <- model$network$Study.order

    #The studies with missing outcomes
    na_studies <- names(outcomes[is.na(outcomes)])
    
    outcomes[na_studies] <- imputed_outcomes[na_studies]
  }
  
  return(outcomes)
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
