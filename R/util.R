#####
# Contains utility functions
# May want to re-organise location later
# File created by NVB
#####

#' Create a bayesian meta-analysis object.
#'
#' @param data Data frame to analyse.
#' @param treatment_list Data frame containing treatment IDs (Number) and names (Label).
#' @param metaoutcome Meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD".
#' @param modelranfix Model effects: either "random" or "fixed".
#' @param reference ID of reference treatment.
#'
#' @return Calculated bayesian meta-analysis.
bayesian_model <- function(data, treatment_list, metaoutcome, 
                           outcome_measure, modelranfix, reference) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  return(baye(longsort2, treatment_list, modelranfix, outcome_measure ,metaoutcome, reference))
}

#' Function to create data regarding rank results - CRN.
#'
#' @param data Data frame to analyse.
#' @param metaoutcome Meta analysis outcome: "Continuous" or "Binary".
#' @param treatment_list Data frame containing treatment IDs (Number) and names (Label).
#' @param bayesmodel Bayesian meta-analysis.
#' @param rankdir Direction of ranking. Either "good" or "bad", describing the desirability of small values.
#'
#' @return Ranking data.
obtain_rank_data <- function(data, metaoutcome, treatment_list, bayesmodel, rankdir) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  # Use the self-defined function, rankdata in fn.analysis.R
  return(rankdata(NMAdata = bayesmodel$mtcResults, rankdirection = rankdir, longdata = longsort2))
}

#' Create the node-split model.
#'
#' @param data Data frame to analyse.
#' @param treatment_list Data frame containing treatment IDs (Number) and names (Label).
#' @param metaoutcome Meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD".
#' @param modelranfix Model effects: either "random" or "fixed".
#'
#' @return Calculated node-split model.
nodesplit <- function(data, treatment_list, metaoutcome, outcome_measure, modelranfix) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  bayenode(longsort2, treatment_list, modelranfix, outcome_measure, metaoutcome)
}

