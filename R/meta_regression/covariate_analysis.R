
#' Find the lowest and highest covariate values given by a study comparing the reference and comparator treatments.
#' This does noes not include studies which do not directly compare treatments to the reference.
#'
#' @param data Data frame from which to find covariate ranges
#' @param treatment_ids data frame containing treatment names ("Label") and IDs ("Number")
#' @param reference Name of reference treatment.
#' @param covariate_title Title of covariate column in data. Only required when @param baseline_risk == FALSE.
#' @param baseline_risk TRUE if the covariate is baseline risk. Defaults to FALSE.
#' @param outcome_type "Binary" or "Continuous". Only required when @param baseline_risk == TRUE. Defaults to NULL.
#' @param model Model created by bnma::network.run(). Only required when @param baseline_risk == TRUE. Defaults to NULL.
#'
#' @return The lowest and highest covariate values of relevant studies. This is structured as a list containing 2 items:
#' - "min" a named vector of the lowest values, where the names are the treatment names.
#' - "max" a named vector of the highest values, where the names are the treatment names.
FindCovariateRanges <- function(data, treatment_ids, reference, covariate_title, baseline_risk = FALSE, outcome_type = NULL, model = NULL) {
  
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
    max_treatments <- max(lengths(study_treatments))
    
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
  
  colnames(study_treatments) <- studies
  
  non_reference_treatment_names <- treatment_ids$Label[treatment_ids$Label != reference]
  
  if (baseline_risk) {
    baseline_risk_covariate <- GetReferenceOutcome(data = data,
                                                   treatment_ids = treatment_ids,
                                                   outcome_type = outcome_type,
                                                   observed = "Imputed",
                                                   model = model)
  }
  
  treatment_min <- c()
  treatment_max <- c()
  for (treatment_name in non_reference_treatment_names) {
    min <- NA
    max <- NA
    for (study in studies) {
      
      if (treatment_name %in% study_treatments[, study] && reference %in% study_treatments[, study]) {
        if (!baseline_risk) {
          covariate_value <- data[[covariate_title]][data$Study == study][1]
        } else {
          covariate_value <- baseline_risk_covariate[study]
        }
        
        if (is.na(min) || min > covariate_value) {
          min <- covariate_value
        }
        
        if (is.na(max) || max < covariate_value) {
          max <- covariate_value
        }
      }
    }
    treatment_min[[treatment_name]] <- unname(min)
    treatment_max[[treatment_name]] <- unname(max)
  }
  
  return(
    list(
      min = unlist(treatment_min),
      max = unlist(treatment_max)
    )
  )
}
