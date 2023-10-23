
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

#' Infer the type of the covariate from the data in the column. In error cases, this
#' function will throw exceptions:
#' - If the data has any NAs
#' - If the data has any non-numeric values
#' - If every study has the same covariate value
#' - If any study contains multiple different covariate values
#'
#' @param data Data frame containing all study data.
#' @param covariate_title Name of the covariate column.
#'
#' @return "binary" if covariate has only 2 numeric values,
#' "continuous" if covariate has more than 2 numeric values
InferCovariateType <- function(data, covariate_title) {
  covariate_data <- data[[covariate_title]]
  
  covariate_values <- list()
  for (study in unique(data$Study)) {
    study_covariate_values <- unique(covariate_data[data$Study == study])
    covariate_values[[study]] <- study_covariate_values[!is.na(study_covariate_values)]
  }
  
  .ThrowErrorForMatchingStudies(
    values = covariate_values,
    condition = function(study_values) {
      length(study_values) == 0
    },
    message = "Some studies do not define covariate values:"
  )
  
  .ThrowErrorForMatchingStudies(
    values = covariate_values,
    condition = function(study_values) {
      length(study_values) > 1
    },
    message = "Some studies contain inconsistent covariate values:"
  )
  
  .ThrowErrorForMatchingStudies(
    values = covariate_values,
    condition = function(study_values) {
      any(!is.numeric(study_values))
    },
    message = "Some studies contain non-numerical covariate values:"
  )

  unique_items <- unique(covariate_data)
  if (length(unique_items) == 1) {
    stop("Cannot analyse covariate with no variation.")
  } else if (length(unique_items) == 2) {
    return("binary")
  } else {
    return("continuous")
  }
}

#' 
#' #' Infer the type of the covariate from the data in the column. In error cases, this
#' #' function will throw exceptions:
#' #' - If the data has any NAs
#' #' - If the data has any non-numeric values
#' #' - If every study has the same covariate value
#' #' - If any study contains multiple different covariate values
#' #'
#' #' @param data Data frame containing all study data.
#' #' @param covariate_title Name of the covariate column.
#' #'
#' #' @return "binary" if covariate has only 2 numeric values,
#' #' "continuous" if covariate has more than 2 numeric values
#' InferCovariateType <- function(data, covariate_title) {
#'   covariate_data <- data[[covariate_title]]
#'   
#'   covariate_values <- list()
#'   inconsistent_studies <- c()
#'   undefined_studies <- c()
#'   for (study in unique(data$Study)) {
#'     study_covariate_values <- unique(covariate_data[data$Study == study])
#'     study_covariate_values <- study_covariate_values[!is.na(study_covariate_values)]
#'     covariate_values[[study]] <- study_covariate_values
#'     if (length(study_covariate_values) == 0) {
#'       undefined_studies <- c(undefined_studies, study)
#'     } else if (length(study_covariate_values) > 1) {
#'       inconsistent_studies <- c(inconsistent_studies, study)
#'     }
#'   }
#'   
#'   if (length(undefined_studies) > 0) {
#'     studies_list <- glue::glue_collapse(undefined_studies, sep = ", ")
#'     stop(paste0("Some studies do not define covariate values: ", studies_list))
#'   }
#'   
#'   if (length(inconsistent_studies) > 0) {
#'     studies_list <- glue::glue_collapse(inconsistent_studies, sep = ", ")
#'     stop(paste0("Some studies contain inconsistent covariate values: ", studies_list))
#'   }
#'   
#'   if (any(!is.numeric(unique_items))) {
#'     stop("Cannot perform meta-regression when covariate values are not numerical.")
#'   } else if (length(unique_items) == 1) {
#'     stop("Cannot analyse covariate with no variation.")
#'   } else if (length(unique_items) == 2) {
#'     return("binary")
#'   } else {
#'     return("continuous")
#'   }
#' }


#' 
#' #' Infer the type of the covariate from the data in the column. In error cases, this
#' #' function will throw exceptions:
#' #' - If the data has any NAs
#' #' - If the data has any non-numeric values
#' #' - If every study has the same covariate value
#' #' - If any study contains multiple different covariate values
#' #'
#' #' @param data Data frame containing all study data.
#' #' @param covariate_title Name of the covariate column.
#' #'
#' #' @return "binary" if covariate has only 2 numeric values,
#' #' "continuous" if covariate has more than 2 numeric values
#' InferCovariateType <- function(data, covariate_title) {
#'   covariate_data <- data[[covariate_title]]
#'   
#'   inconsistent_studies <- c()
#'   for (study in unique(data$Study)) {
#'     study_covariate_values <- unique(covariate_data[data$Study == study])
#'     study_covariate_values <- study_covariate_values[!is.na(study_covariate_values)]
#'     if (length(study_covariate_values) > 1) {
#'       inconsistent_studies <- c(inconsistent_studies, study)
#'     }
#'   }
#'   
#'   if (length(inconsistent_studies) > 0) {
#'     studies_list <- glue::glue_collapse(inconsistent_studies, sep = ", ")
#'     stop(paste0("Some studies contain inconsistent covariate values: ", studies_list))
#'   }
#'   
#'   unique_items <- unique(covariate_data)
#'   
#'   if (any(is.na(unique_items))) {
#'     stop("Cannot perform meta-regression when some studies are missing the covariate.")
#'   } else if (any(!is.numeric(unique_items))) {
#'     stop("Cannot perform meta-regression when covariate values are not numerical.")
#'   } else if (length(unique_items) == 1) {
#'     stop("Cannot analyse covariate with no variation.")
#'   } else if (length(unique_items) == 2) {
#'     return("binary")
#'   } else {
#'     return("continuous")
#'   }
#' }
