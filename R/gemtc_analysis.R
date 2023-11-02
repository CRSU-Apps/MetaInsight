# User-written functions to wrap up functionality related to running models with {gemtc} #


#' Function for formatting standard app user data ready for start of {gemtc} chain of commands
#' 
#' @param data Uploaded data from the user (or in-built datasets)
#' @param outcome_type Indicator of whether data is binary or continuous
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @return list containing two dataframes: armData containing the core data; studyData containing covariate data
PrepDataGemtc <- function(data, outcome_type, covariate, cov_friendly){
  # ensure data is in long format
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome_type)
  } else {
    long_data <- data
  }
  # specify arm level data
  if (outcome_type == "Continuous") {
    armData <- data.frame(study = long_data$Study,
                          treatment = long_data$T,
                          mean = long_data$Mean,
                          std.dev = long_data$SD,
                          sampleSize = long_data$N)
  } else {
    armData <- data.frame(study = long_data$Study,
                          treatment = long_data$T,
                          responders = long_data$R,
                          sampleSize = long_data$N)
  }
  # specify study level data
  studyData <- unique(data.frame(study = long_data$Study,
                                 cov_name = long_data[,covariate]))
  names(studyData)[2] <- cov_friendly
  rownames(studyData) <- NULL
  
  return(list(armData = armData, studyData = studyData))
}

#' Function to wrap up {gemtc} commands for running a covariate model
#' 
#' @param data list containing armData and studyData (as created by PrepDataGemtc)
#' @param model_type Whether the model is 'fixed' or 'random'
#' @param regressor_type Type of regression coefficient, either "shared", "unrelated", or "exchangeable"
#' @param ref_choice The choice of reference treatment as selected by user
#' @return An object of class mtc.result which can be used for further output functions such as summary() or plots
RunCovariateRegression <- function(data, model_type, regressor_type, ref_choice) {
  # Create 'network' object
  network_object <- gemtc::mtc.network(data.ab = data$armData,
                                       studies = data$studyData)
  # Define regression coefficient
  regressor <- list(coefficient=regressor_type,
                    variable=colnames(data$studyData[2]),
                    control=ref_choice)
  # Create 'model' object
  set.seed(145) # needs to be set before mtc.model
  model_object <- gemtc::mtc.model(network_object,
                                   type = "regression",
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
  
  # Run model
  model_output <- gemtc::mtc.run(model_object)
  
  return(model_output)
}
