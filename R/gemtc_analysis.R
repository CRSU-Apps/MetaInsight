# User-written functions to wrap up functionality related to running models with {gemtc} #


#' Function for formatting standard app user data into a {gemtc} 'network' object
#' 
#' @param data Uploaded data from the user (or in-built datasets)
#' @param outcome_type Indicator of whether data is binary or continuous
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly version of chosen covariate
#' @return

CreateGemtcObject <- function(data, outcome_type, covariate, cov_friendly){
  # ensure data is in long format
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome_type)
  } else {
    long_data <- data
  }
  # specify arm level data
  if (outcome_type == "Continuous") {
    armData <- data.frame(study=long_data$Study,
                          treatment=long_data$T,
                          mean=long_data$Mean,
                          std.dev=long_data$SD,
                          sampleSize=long_data$N)
  } else {
    armData <- data.frame(study=long_data$Study,
                          treatment=long_data$T,
                          responders=long_data$R,
                          sampleSize=long_data$N)
  }
  # specify study level data
  studyData <- unique(data.frame(study=long_data$Study,
                                 cov_name = long_data[,covariate]))
  names(studyData)[2] <- cov_friendly
  
  return(mtc.network(data.ab=armData,studies=studyData))
}