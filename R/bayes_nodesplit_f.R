#' Run the nodesplit model
#'
#' @param data Data to analyse.
#' @param treatment_df Dataframe containing treatment names ("Label") and IDs ("Number").
#' @param outcome character. The type of outcome being measured either `Continuous` or `Binary`
#' @param outcome_measure character. The analysis outcome measure. Either `MD`, `OR` or `RR`
#' @param model_type character. The type of model. Either `random` or `fixed`
#'
#' @return The created nodesplit model
#' @export
bayes_nodesplit <- function(data, treatment_df, outcome, outcome_measure, model_type, async = FALSE) {

  if (outcome_measure == "SMD" ) {
    return(async |> asyncLog(type = "error", "Standardised mean difference currently cannot be analysed in Bayesian analysis"))
  }
  else if (outcome_measure == "RD") {
    return(async |> asyncLog(type = "error", "Bayesian analysis of risk differences is not currently implemented in MetaInsight"))
  }
  if (!outcome_measure %in% c('OR', 'RR', 'MD')) {
    return(async |> asyncLog(type = "error", glue::glue("Outcome_measure type '{outcome_measure}' is not supported. Please use one of: 'MD', 'OR', 'RR'")))
  }

  data <- dataform.df(data, treatment_df, outcome)
  
  if (outcome == "Continuous") {
    armData <- data.frame(
      study = data$Study,
      treatment = data$T,
      mean = data$Mean,
      std.err = data$se
    )
  } else {
    armData <- data.frame(
      study = data$Study,
      treatment = data$T,
      responders = data$R,
      sampleSize = data$N
    )
  }
  mtcNetwork <- gemtc::mtc.network(
    data.ab = armData,
    description = "Network",
    treatments = data.frame(
      id = treatment_df$Label,
      description = treatment_df$Label
    )
  )
  
  if (outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else  {
    like <- "binom"
    link <- ifelse (outcome_measure == "OR", "logit", "log")
  }

  check_nodesplit <- IsNodesplittable(
    data = data,
    treatments = treatment_df$Label
  )
  
  if (!check_nodesplit$is_nodesplittable) {
    return(
      async |> asyncLog(
        type = "error",
        check_nodesplit$reason
      )
    )
  } else {
    return(
      gemtc::mtc.nodesplit(
        network = mtcNetwork,
        linearModel = model_type,
        likelihood = like,
        link = link
      )
    )
  }
}
