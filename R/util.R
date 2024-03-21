#####
# Contains utility functions
# May want to re-organise location later
# File created by NVB
#####

#' Bayesian analysis
#' 
#' @param sub TRUE for sensitivity analysis, FALSE for full analysis.
#' @param data Input data set.
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param metaoutcome "Continuous" or "Binary".
#' @param exclusionbox Vector of studies excluded from the sensitivity analysis.
#' @param outcome_measure "MD", "OR" or "RR".
#' @param modelranfix "fixed" or "random".
#' @param reference_alter List of reference treatments
#'  - 'ref_all': Reference treatment for the full analysis.
#'  - 'ref_sub': Reference treatment for the sensitivity analysis.
#' @return Output created by baye().
bayesian_model <- function(
    sub,
    data,
    treatment_list,
    metaoutcome,
    exclusionbox,
    outcome_measure,
    modelranfix,
    reference_alter) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome) 
  if (sub) {
    return(
      baye(
        filter(longsort2, !Study %in% exclusionbox),
        treatment_list,
        modelranfix,
        outcome_measure,
        metaoutcome,
        reference_alter$ref_sub
      )
    )
  } else {
    return(
      baye(
        longsort2,
        treatment_list,
        modelranfix,
        outcome_measure,
        metaoutcome,
        reference_alter$ref_all
      )
    )
  }
}



#' Function to create data regarding rank results - CRN
#' 
#' @param data Input data set.
#' @param metaoutcome "Continuous" or "Binary".
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param bayesmodel List of various model output created by baye().
#' @param rankdir "good" or "bad", referring to small outcome values.
#' @param excluded Vector of excluded studies for sensitivity analysis.
#' @return List of output created by rankdata().
obtain_rank_data <- function(data, metaoutcome, treatment_list, bayesmodel, rankdir, excluded = c()) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  if (length(excluded > 0)) {
    # Subset of data when studies excluded
    longsort2 <- dplyr::filter(longsort2, !Study %in% excluded)
  }
  # Use the self-defined function, rankdata() in bayes_analysis.R
  return(
    rankdata(
      NMAdata = bayesmodel$mtcResults,
      rankdirection = rankdir, 
      longdata = longsort2
    )
  )
}



#' Run the nodesplit model
#'
#' @param data Data to analyse.
#' @param treatment_list Data frame containing treatment names ("Label") and IDs ("Number").
#' @param metaoutcome "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR" or "RR".
#' @param modelranfix The type of model. Either "random" or "fixed".
#' @param exclusions Vector of excluded studies. Defaults to empty vector.
#'
#' @return The created nodesplit model, created in bayenode().
nodesplit <- function(data, treatment_list, metaoutcome, outcome_measure, modelranfix, exclusions = c()) {
  newData1 <- as.data.frame(data)
  longsort2 <- filter(
    dataform.df(newData1, treatment_list, metaoutcome),
    !Study %in% exclusions
  )
  bayenode(data = longsort2, treat_list = treatment_list, model = modelranfix,
           outcome = outcome_measure, CONBI = metaoutcome)
}
