#####
# Contains utility functions
# May want to re-organise location later
# File created by NVB
#####

# Bayesian analysis
bayesian_model <- function(sub, data, treatment_list, metaoutcome, exclusionbox, 
                           outcome_measure, modelranfix, reference_alter) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome) 
  if (sub == TRUE) {
    longsort2 <- filter(longsort2, !Study %in% exclusionbox)
    return(baye(longsort2, treatment_list, modelranfix, outcome_measure ,metaoutcome, 
                reference_alter$ref_sub))
  } else {
    return(baye(longsort2, treatment_list, modelranfix, outcome_measure ,metaoutcome, 
                reference_alter$ref_all))
  }
}

# Function to create data regarding rank results - CRN
obtain_rank_data <- function(data, metaoutcome, treatment_list, bayesmodel, rankdir, excluded = c()) {
  newData1 <- as.data.frame(data)
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  if (length(excluded > 0)) {
    # Subset of data when studies excluded
    longsort2 <- dplyr::filter(longsort2, !Study %in% excluded)
  }
  # Use the self-defined function, rankdata in fn.analysis.R
  return(rankdata(NMAdata=bayesmodel$mtcResults, rankdirection=rankdir, 
           longdata=longsort2))
}

#' Run the nodesplit model
#'
#' @param data Data to analyse.
#' @param treatment_list Data frame containing treatment names ("Label") and IDs ("Number").
#' @param metaoutcome The type of outcome being measured.
#' @param outcome_measure The analysis outcome measure.
#' @param modelranfix The type of model. Either "random" or "fixed"
#' @param exclusions Vector of excluded studies. Defaults to empty vector.
#'
#' @return The created nodesplit model
nodesplit <- function(data, treatment_list, metaoutcome, outcome_measure, modelranfix, exclusions = c()) {
  newData1 <- as.data.frame(data)
  longsort2 <- filter(
    dataform.df(newData1, treatment_list, metaoutcome),
    !Study %in% exclusions
  )
  bayenode(longsort2, treatment_list, modelranfix, outcome_measure, metaoutcome)
}

