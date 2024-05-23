# User-written functions to wrap up functionality related to running models with {gemtc} #


#' Function for formatting standard app user data ready for start of {gemtc} chain of commands
#' 
#' @param data Uploaded data post-processing
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome_type Indicator of whether data is 'Binary' or 'Continuous'
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @return list containing two dataframes: armData containing the core data; studyData containing covariate data
PrepDataGemtc <- function(data, treatment_ids, outcome_type, covariate, cov_friendly){
  # ensure data is in long format
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome_type)
  } else {
    long_data <- data
  }
  # specify arm level data
  if (outcome_type == "Continuous") {
    armData <- data.frame(study = long_data$Study,
                          treatment = treatment_ids$Label[match(long_data$T, treatment_ids$Number)],
                          mean = long_data$Mean,
                          std.dev = long_data$SD,
                          sampleSize = long_data$N)
  } else if (outcome_type == "Binary") {
    armData <- data.frame(study = long_data$Study,
                          treatment = treatment_ids$Label[match(long_data$T, treatment_ids$Number)],
                          responders = long_data$R,
                          sampleSize = long_data$N)
  } else {
    paste0("Outcome_type has to be 'Continuous' or 'Binary'")
  }
  # specify study level data
  studyData <- unique(data.frame(study = long_data$Study,
                                 covariate = long_data[, covariate]))
  names(studyData)[2] <- cov_friendly
  rownames(studyData) <- NULL
  
  return(list(armData = armData, studyData = studyData))
}

#' Function for setting up covariate {gemtc} model object
#' 
#' @param data list containing armData and studyData (as created by PrepDataGemtc)
#' @param model_type Whether the model is 'fixed' or 'random'
#' @param outcome Type of outcome measure ('OR', 'RR', or 'MD')
#' @param regressor_type Type of regression coefficient, either "shared", "unrelated", or "exchangeable"
#' @param ref_choice The choice of reference treatment as selected by user
#' @return An object of class mtc.model 
CreateGemtcModel <- function(data, model_type, outcome, regressor_type, ref_choice) {
  # Create 'network' object
  network_object <- gemtc::mtc.network(data.ab = data$armData,
                                       studies = data$studyData)
  # Define regression coefficient
  regressor <- list(coefficient=regressor_type,
                    variable=colnames(data$studyData[2]),
                    control=ref_choice)
  # Create 'model' object
  set.seed(145) # needs to be set before mtc.model
  if (outcome == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome %in% c('OR', 'RR')) {
    like <- "binom"
    if (outcome == "OR") {
      link <- "logit"
    } else if (outcome == "RR") {
      link <- "log"
    } 
  } else {
    paste0("Outcome can only be OR, RR, or MD")
  }
  model_object <- gemtc::mtc.model(network_object,
                                   type = "regression",
                                   likelihood=like,
                                   link = link,
                                   linearModel = model_type,
                                   regressor = regressor)
  # Settings for JAGS seeds and generator types for reproducible results (code taken from mtc.model manual)
  seeds <- sample.int(4, n = .Machine$integer.max) # 4 chains
  model_object$inits <- mapply(c, model_object$inits, list(
    list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[1]),
    list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = seeds[2]),
    list(.RNG.name = "base::Super-Duper", .RNG.seed = seeds[3]),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seeds[4])),
    SIMPLIFY = FALSE)
  
  return(model_object)
}


#' Function for running covariate model
#' 
#' @param data Uploaded data post-processing
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome_type Indicator of whether data is 'Binary' or 'Continuous'
#' @param outcome Type of outcome measure ('OR', 'RR', or 'MD')
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @param model_type Whether the model is 'fixed' or 'random'
#' @param regressor_type Type of regression coefficient, either "shared", "unrelated", or "exchangeable"
#' @param ref_choice The choice of reference treatment as selected by user
#' @return An object of class mtc.result which can be used for further output functions such as summary() or plots
RunCovariateModel <- function(data, treatment_ids, outcome_type, outcome, covariate, cov_friendly, model_type, regressor_type, ref_choice) {
  prepped_data <- PrepDataGemtc(data, treatment_ids, outcome_type, covariate, cov_friendly)
  gemtc_model <- CreateGemtcModel(prepped_data, model_type, outcome, regressor_type, ref_choice)
  model_output <- gemtc::mtc.run(gemtc_model)
  
  return(model_output)
}


#' Function to collate all the model output to be used in other existing functions
#' 
#' @param data Data frame from which model was calculated
#' @param treatment_ids Data frame containing treatment IDs (Number) and names (Label)
#' @param model Completed model object after running RunCovariateRegression()
#' @param covariate_title Covariate name as per uploaded data
#' @param cov_value Value of covariate for which to give output (default value the mean of study covariates)
#' @param outcome_measure The outcome measure for the analysis: One of: "OR", "RR", "MD"
#' @return List of gemtc related output:
#'  mtcResults = model object itself carried through (needed to match existing code)
#'  mtcRelEffects = data relating to presenting relative effects;
#'  rel_eff_tbl = table of relative effects for each comparison;
#'  covariate_value = The covariate value originally passed into this function
#'  reference_name = The name of the reference treatment
#'  comparator_names = Vector containing the names of the comparators.
#'  a = text output stating whether fixed or random effects;
#'  sumresults = summary output of relative effects
#'  dic = data frame of model fit statistics
#'  cov_value_sentence = text output stating the value for which the covariate has been set to for producing output
#'  slopes = named list of slopes for the regression equations (unstandardised - equal to one 'increment')
#'  intercepts = named list of intercepts for the regression equations at cov_value
#'  outcome = The outcome type for the analysis eg. "MD" or "OR"
#'  mtcNetwork = The network object from GEMTC
#'  model = The type of linear model, either "fixed" or "random"
#'  covariate_min = Vector of minimum covariate values directly contributing to the regression.
#'  covariate_max = Vector of maximum covariate values directly contributing to the regression.
CovariateModelOutput <- function(data, treatment_ids, model, covariate_title, cov_value, outcome_measure) {
  
  model_levels = levels(model$model$data$reg.control)
  reference_name <- model_levels[model_levels %in% model$model$data$reg.control]
  comparator_names <- model_levels[!model_levels %in% model$model$data$reg.control]
  
  # Create text for random/fixed effect
  model_text <- paste(model$model$linearModel, "effect", sep = " ")
  
  # Relative Effects raw data
  rel_eff <- gemtc::relative.effect(model, as.character(model$model$regressor$control), covariate = cov_value)
  
  # Summary of relative effects
  rel_eff_summary <- summary(rel_eff)
  
  # Relative Effects table of all comparisons
  rel_eff_tbl <- gemtc::relative.effect.table(model, covariate = cov_value)
  
  # Table of Model fit stats
  fit_stats <- as.data.frame(summary(model)$DIC)
  
  # Summary sentence of where covariate value has been set for results
  cov_value_sentence <- paste0("Value for covariate ", model$model$regressor$variable, " set at ", cov_value)
  
  # Obtain slope(s)
  slope_indices <- grep(ifelse(model$model$regressor$coefficient == "shared", "^B$", "^beta\\[[0-9]+\\]$"), model$model$monitors$enabled)
  model_summary <- summary(model)
  slopes <- model_summary$summaries$quantiles[slope_indices, "50%"] / model$model$regressor$scale
  
  # Duplicate slope for each comparator when "shared" type
  if (model$model$regressor$coefficient == "shared") {
    slopes <- rep(slopes[1], length(comparator_names))
  }
  
  # Intercepts (regression)
  rel_eff_zero <- gemtc::relative.effect(model, as.character(model$model$regressor$control), covariate = 0)
  rel_eff_zero_summary <- summary(rel_eff_zero)
  intercepts <- rel_eff_zero_summary$summaries$quantiles[startsWith(rownames(rel_eff_zero_summary$summaries$quantiles), "d."), "50%"]
  
  # Rename items for intercepts and slopes
  names(slopes) <- comparator_names
  names(intercepts) <- comparator_names
  
  min_max <- FindCovariateRanges(
    data = data,
    treatment_ids = treatment_ids,
    reference = as.character(model$model$regressor$control),
    covariate_title = covariate_title
  )

  # naming conventions to match current Bayesian functions
  return(
    list(
      mtcResults = model,
      mtcRelEffects = rel_eff,
      rel_eff_tbl = rel_eff_tbl,
      covariate_value = cov_value,
      reference_name = reference_name,
      comparator_names = comparator_names,
      a = model_text,
      sumresults = rel_eff_summary,
      dic = fit_stats,
      cov_value_sentence = cov_value_sentence,
      slopes = slopes,
      intercepts = intercepts,
      outcome = outcome_measure,
      mtcNetwork = model$model$network,
      model = model$model$linearModel,
      covariate_min = min_max$min,
      covariate_max = min_max$max
    )
  )
}

#' Function to find default covariate value of regression model
#' @param model meta-regression model object (from gemtc)
#' 
#' @return default value
FindCovariateDefault <- function(model) {
  if (model$model$regressor$type == "continuous") {
    cov_value <- model$model$regressor$center # center value
  } else if (model$model$regressor$type == "binary") {
    cov_value <- 0 # base group
  } else {
    stop("regressor type needs to be 'continuous' or 'binary'")
  }
  return(cov_value)
}

#' Calculate the confidence regions within direct evidence for the regression model.
#'
#' @param model_output Return from `CovariateModelOutput()`.
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
CalculateConfidenceRegions <- function(model_output) {
  mtc_results <- model_output$mtcResults
  reference_name <- model_output$reference_name
  
  confidence_regions <- list()
  confidence_intervals <- list()
  
  for (treatment_name in model_output$comparator_names) {
    parameter_name <- glue::glue("d.{reference_name}.{treatment_name}")
    cov_min <- model_output$covariate_min[treatment_name]
    cov_max <- model_output$covariate_max[treatment_name]
    
    if (is.na(cov_min)) {
      confidence_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else if (cov_min == cov_max) {
      interval <- .FindConfidenceInterval(mtc_results, reference_name, cov_min, parameter_name)
      df <- data.frame(cov_value = cov_min, lower = interval["2.5%"], upper = interval["97.5%"])
      
      # Strip out the row names
      rownames(df) <- NULL
      
      # Add to regions list
      confidence_intervals[[treatment_name]] <- df
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else {
      df <- data.frame()
      for (cov_value in seq(from = cov_min, to = cov_max, length.out = 11)) {
        interval <- .FindConfidenceInterval(mtc_results, reference_name, cov_value, parameter_name)
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
.FindConfidenceInterval <- function(mtc_results, reference_name, cov_value, parameter_name) {
  rel_eff <- gemtc::relative.effect(mtc_results, reference_name, covariate = cov_value)
  rel_eff_summary <- summary(rel_eff)
  return(rel_eff_summary$summaries$quantiles[parameter_name, c("2.5%", "97.5%")])
}
