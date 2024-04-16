
#' Find the lowest and highest covariate values given by a study comparing the reference and comparator treatments.
#' This does noes not include studies which do not directly compare treatments to the reference.
#'
#' @param data Data frame from which to find covariate ranges
#' @param treatment_ids data frame containing treatment names ("Label") and IDs ("Number")
#' @param reference Name of reference treatment.
#' @param covariate_title Title of covariate column in data.
#'
#' @return The lowest and highest covariate values of relevant studies. This is structured as a list containing 2 items:
#' - "min" a named vector of the lowest values, where the names are the treatment names.
#' - "max" a named vector of the highest values, where the names are the treatment names.
FindCovariateRanges <- function(data, treatment_ids, reference, covariate_title) {
  studies <- unique(data$Study)
  
  study_treatments <- sapply(
    studies,
    function(study) {
      FindAllTreatments(data, treatment_ids, study)
    }
  )
  
  # Turn list into matrix
  # This is only needed when there are different numbers of treatment arms between studies
  if (is.list(study_treatments)) {
    max_treatments <- max(
      sapply(
        names(study_treatments),
        function(name) {
          length(study_treatments[[name]])
        }
      )
    )
    
    temp_matrix <- matrix(
      nrow = max_treatments,
      ncol = length(studies)
    )
    colnames(temp_matrix) <- studies
    
    sapply(
      studies,
      function(study) {
        treatments <- study_treatments[[study]]
        temp_matrix[1:length(treatments), study] <<- treatments
      }
    )
    
    study_treatments <- temp_matrix
  }
  
  non_reference_treatment_names <- treatment_ids$Label[treatment_ids$Label != reference]
  
  treatment_min <- c()
  treatment_max <- c()
  for (treatment_name in non_reference_treatment_names) {
    min <- NA
    max <- NA
    for (study in studies) {
      
      if (treatment_name %in% study_treatments[, study] && reference %in% study_treatments[, study]) {
        covariate_value <- data[[covariate_title]][data$Study == study][1]
        
        if (is.na(min) || min > covariate_value) {
          min <- covariate_value
        }
        
        if (is.na(max) || max < covariate_value) {
          max <- covariate_value
        }
      }
    }
    treatment_min[[treatment_name]] <- min
    treatment_max[[treatment_name]] <- max
  }
  
  return(
    list(
      min = unlist(treatment_min),
      max = unlist(treatment_max)
    )
  )
}
