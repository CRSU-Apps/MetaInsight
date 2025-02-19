
#' Determine whether comparisons contribute to relative treatment effects directly, indirectly, or not at all.
#' 
#' @param data Input data in long format.
#' @param covariate_title Title of covariate column in data. Enter NULL if there is no covariate.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR", "RR" or "RD".
#' @param effects_type "fixed" or "random".
#' @return List of contributions:
#' - "direct"
#'   - Matrix containing TRUE, FALSE, or NA corresponding to direct, indirect, or no contribution. Rows are studies, columns are treatments
#' - "relative_effect"
#'   - Matrix of relative effects of treatments compared to the reference. Rows are studies, columns are treatments
#' - "covariate_value"
#'   - Vector of covariate values from the studies.
CalculateDirectness <- function(
    data,
    covariate_title,
    treatment_ids,
    outcome_type,
    outcome_measure,
    effects_type) {

  if (outcome_type == "Binary") {
    d0 <- meta::pairwise(treat = T, event = R, studlab = Study, n = N, data = data, sm = outcome_measure)
  } else if (outcome_type == "Continuous") {
    d0 <- meta::pairwise(treat = T, mean = Mean, sd = SD, studlab = Study, n = N, data = data, sm = outcome_measure)
  } else {
    stop(glue::glue("Outcome type '{outcome_type}' is not supported. Please use 'Binary' or 'Continuous'"))
  }
  
  #Switch the treatment effects to match the rest of the app.
  d0$TE <- -d0$TE
  
  reference_index <- 1
  reference <- treatment_ids$Label[treatment_ids$Number == reference_index]
  treatments <- treatment_ids$Label[treatment_ids$Label != reference]
  studies <- unique(data$Study)
  
  directness <- matrix(
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )

  #Populate 'directness'
  for (study in studies) {
    study_treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids, study = study)
    study_treatment_index <- match(study_treatments, treatment_ids$Label)
    for (treatment in treatments) {
      #If the study contains both the reference and this treatment then it makes a direct contribution
      if (all(c(reference, treatment) %in% study_treatments)) {
        directness[study, treatment] <- TRUE 
      } else {
        treatment_number <- treatment_ids$Number[treatment_ids$Label == treatment]
        #Create the network excluding the edge between the reference and this treatment
        network <- CreateGraph(data = data, exclude_link = c(1, treatment_number))
        #Find all paths between the reference and this treatment
        # All such paths are indirect because the direct path has been excluded
        paths <- tryCatch(igraph::all_simple_paths(graph = network,
                                                   from = 1,
                                                   to = treatment_number),
                          error = function(condition) {
                            return(NULL)
                          })
        if (!is.null(paths) & length(paths) != 0) {
          #If there are paths, check that at least one of them contains at least two treatments from this study
          # If so, then this study contributes to an indirect path between the reference and this treatment
          for (path_number in 1:length(paths)) {
            if (sum(study_treatment_index %in% paths[[path_number]]) >= 2) {
              directness[study, treatment] <- FALSE
              break
            }
          }
        }
      }
    }
  }

  relative_effects <- matrix( 
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )
  
  #Populate 'relative_effects'
  for (study in studies) {
    for (treatment in treatments) {
      treatment_index <- treatment_ids$Number[treatment_ids$Label == treatment]
      treatment_effect <- d0$TE[d0$treat1 == reference_index & d0$treat2 == treatment_index & d0$Study == study]
      
      if (length(treatment_effect) != 0) {
        relative_effects[study, treatment] <- treatment_effect
      }
    }
  }
  
  covariate_values <- data[[covariate_title]][match(studies, data$Study)]
  names(covariate_values) <- studies

  return(
    list(
      direct = directness,
      relative_effect = relative_effects,
      covariate_value = covariate_values
    )
  )
}
