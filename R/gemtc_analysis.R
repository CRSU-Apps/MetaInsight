# User-written functions to wrap up functionality related to running models with {gemtc} #


#' Function for formatting standard app user data into a {gemtc} 'network' object
#' 
#' @param data Uploaded data from the user (or in-built datasets)
#' @param ConBi Indicator of whether data is binary or continuous
#' @return

CreateGemtcObject <- function(data, ConBi){
  # ensure data is in long format
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, ConBi)
  } else {
    long_data <- data
  }
  # specify arm level data
  if (ConBi == "Continuous") {
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
                                 covariate_name = long_data[,FindCovariateNames(long_data)[1]]))
  names(studyData)[2] <- GetFriendlyCovariateName(FindCovariateNames(long_data)[1])
  
  return(mtc.network(data.ab=armData,studies=studyData))
}
