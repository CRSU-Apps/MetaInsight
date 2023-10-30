
#' Throw an error if any study meets given criteria.
#'
#' @param values covariate values in which to check.
#' @param condition Function taking the covariate values to check.
#' This should return TRUE in the error case.
#' @param message Message to show before listing the problem studies.
.ThrowErrorForMatchingStudies <- function(values, condition, message) {
  study_conditions <- sapply(
    names(values),
    function(name) {
      condition(values[[name]])
    }
  )
  
  matching_studies <- names(study_conditions)[study_conditions]
  
  if (length(matching_studies) > 0) {
    studies_list <- glue::glue_collapse(matching_studies, sep = ", ")
    stop(paste(message, studies_list, sep = " "))
  }
}

#' Validate the covariate and infer the type of the covariate from the data in the column.
#'  In error cases, this function will throw exceptions:
#' - If the data has any NAs
#' - If the data has any non-numeric values
#' - If every study has the same covariate value
#' - If any study contains multiple different covariate values
#'
#' @param data Data frame containing all study data.
#' @param covariate_title Name of the covariate column.
#'
#' @return "binary" if covariate has only 0 & 1 as numeric values,
#' "continuous" if covariate has more than 2 numeric values
ValidateAndInferCovariateType <- function(data, covariate_title) {
  covariate_data <- data[[covariate_title]]
  
  if (!is.numeric(covariate_data)) {
    stop("One or more covariate values are non-numerical.")
  }
  
  covariate_values <- list()
  for (study in unique(data$Study)) {
    covariate_values[[study]] <- unique(covariate_data[data$Study == study])
  }
  
  .ThrowErrorForMatchingStudies(
    values = covariate_values,
    condition = function(study_values) {
      any(is.na(study_values))
    },
    message = "Some studies do not define covariate values for all arms:"
  )
  
  .ThrowErrorForMatchingStudies(
    values = covariate_values,
    condition = function(study_values) {
      length(study_values) > 1
    },
    message = "Some studies contain inconsistent covariate values between arms:"
  )
  
  unique_items <- unique(covariate_data)
  if (length(unique_items) == 1) {
    stop("Cannot analyse covariate with no variation.")
  } else if (length(unique_items) == 2 && all(sort(unique_items) == c(0, 1))) {
    return("binary")
  }
  
  return("continuous")
}
