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
                                 cov_name = long_data[,covariate]))
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
#' @param model Completed model object after running RunCovariateRegression()
#' @param cov_value Value of covariate for which to give output (default value the mean of study covariates)
#' @return List of gemtc related output:
#'  mtcResults = model object itself carried through (needed to match existing code)
#'  mtcRelEffects = data relating to presenting relative effects;
#'  covariate_value = The covariate value orignally passed into this function
#'  reference_name = The nmae fo the reference treatment
#'  comparator_names = The name of the 
#'  a = text output stating whether fixed or random effects;
#'  sumresults = summary output of relative effects
#'  dic = data frame of model fit statistics
#'  cov_value_sentence = text output stating the value for which the covariate has been set to for producing output
#'  slopes = named list of slopes for the regression equations (unstandardised - equal to one 'increment')
#'  intercepts = named list of intercepts for the regression equations at cov_value
CovariateModelOutput <- function(model, cov_value) {
  
  model_levels = levels(model$model$data$reg.control)
  reference_name <- model_levels[model_levels %in% model$model$data$reg.control]
  comparator_names <- model_levels[!model_levels %in% model$model$data$reg.control]
  
  
  # Relative Effects raw data
  rel_eff <- gemtc::relative.effect(model, as.character(model$model$regressor$control), covariate = cov_value)
  
  # Create text for random/fixed effect
  model_text <- paste(model$model$linearModel,"effect",sep=" ")
  
  # Summary of relative effects
  summary <- summary(rel_eff)
  
  # Intercepts (regression)
  intercepts <- summary$summaries$statistics[1:(nrow(summary$summaries$statistics)-1),1]
  
  # Table of Model fit stats
  fit_stats <- as.data.frame(summary$DIC)
  
  # Summary sentence of where covariate value has been set for results
  cov_value_sentence <- paste0("Value for covariate ", model$model$regressor$variable, " set at ", cov_value)
  
  # Obtain slope(s)
  slope_indices <- grep(ifelse(model$model$regressor$coefficient == "shared", "^B$", "^beta\\[[0-9]+\\]$"), model$model$monitors$enabled)
  summ <- summary(model)
  slopes <- summ$summaries$statistics[slope_indices, 1]  * model$model$regressor$scale
  
  # Rename rows for intercepts and slopes
  if (model$model$regressor$coefficient == "shared") {
    slopes <- rep(slopes[1], length(comparator_names))
  }
  names(slopes) <- comparator_names
  names(intercepts) <- comparator_names
  
  # naming conventions to match current Bayesian functions
  return(
    list(
      mtcResults = model,
      mtcRelEffects = rel_eff,
      covariate_value = cov_value,
      reference_name = reference_name,
      comparator_names = comparator_names,
      a = model_text,
      sumresults = summary,
      dic = fit_stats,
      cov_value_sentence = cov_value_sentence,
      slopes = slopes,
      intercepts = intercepts
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
