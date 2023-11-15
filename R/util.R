#####
# Contains utility functions
# May want to re-organise location later
# File created by NVB
#####


#' Create a bayesian meta-analysis object.
#'
#' @param data Data frame to analyse.
#' @param treatment_list 
#' @param metaoutcome 
#' @param outcome_measure 
#' @param modelranfix 
#' @param reference ID of reference treatment.
#'
#' @return Calculated bayesian meta-analysis.
bayesian_model <- function(data, treatment_list, metaoutcome, 
                           outcome_measure, modelranfix, reference) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  return(baye(longsort2, treatment_list, modelranfix, outcome_measure ,metaoutcome, reference))
}

#' Function to create data regarding rank results - CRN
#'
#' @param data 
#' @param metaoutcome 
#' @param treatment_list 
#' @param bayesmodel 
#' @param rankdir 
#'
#' @return
obtain_rank_data <- function(data, metaoutcome, treatment_list, bayesmodel, rankdir) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  # Use the self-defined function, rankdata in fn.analysis.R
  return(rankdata(NMAdata = bayesmodel$mtcResults, rankdirection = rankdir, longdata = longsort2))
}

# Nodesplit model

#' Create the node-split model.
#'
#' @param data 
#' @param treatment_list 
#' @param metaoutcome 
#' @param outcome_measure 
#' @param modelranfix 
#'
#' @return
nodesplit <- function(data, treatment_list, metaoutcome, outcome_measure, modelranfix) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  bayenode(longsort2, treatment_list, modelranfix, outcome_measure, metaoutcome)
}

