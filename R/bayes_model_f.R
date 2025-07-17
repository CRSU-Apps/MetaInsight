#' Run Bayesian models
#'
#' @param data dataframe. Input data set.
#' @param treatment_df Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param outcome "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR" or "RR".
#' @param model_type "fixed" or "random".
#' @param reference_treatment Reference treatment
#' @param async Whether or not the function is being used aysnchronously. Default `FALSE`
#' @return List:
#'  - 'mtcResults' = Output from gemtc::mtc.run
#'  - 'mtcRelEffects' = Output from gemtc::relative.effect
#'  - 'rel_eff_tbl = Output from gemtc::relative.effect.table
#'  - 'sumresults' = summary(mtcRelEffects)
#'  - 'mtcNetwork' = Output from gemtc::mtc.network
#'  - 'dic' = Data frame containing the statistics 'Dbar', 'pD', 'DIC', and 'data points'
#' @export

bayes_model <- function(data, treatment_df, outcome, outcome_measure, model_type, reference_treatment, async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("data", "treatment_df", "outcome", "outcome_measure", "model_type",  "reference_treatment"),
                            c("data.frame", "data.frame", "character", "character", "character", "character"), NULL)){
      return()
    }
  }

  if (!model_type %in% c("fixed", "random")){
    return(async |> asyncLog(type = "error", "model_type must be 'fixed' or 'random'"))
  }

  if (!outcome_measure %in% c("OR", "RR", "MD")){
    return(async |> asyncLog(type = "error", "outcome_measure must be 'OR', 'RR' or 'MD'"))
  }

  longsort <- dataform.df(data, treatment_df, outcome)

  # Create arm level data set for gemtc
  if (outcome == "Continuous") {
    armData <- data.frame(study = longsort$Study,
                          treatment = longsort$T,
                          mean = longsort$Mean,
                          std.err = longsort$se)
  } else if (outcome == "Binary") {
    armData <- data.frame(study = longsort$Study,
                          treatment = longsort$T,
                          responders = longsort$R,
                          sampleSize = longsort$N)
  }
  # Gemtc network object
  mtcNetwork <- gemtc::mtc.network(data.ab = armData, description = "Network")

  if (outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome_measure == "OR" || outcome_measure == "RR") {
    like <- "binom"
    link <- ifelse (outcome_measure == "OR", "logit", "log")
  }

  mtcModel <- gemtc::mtc.model(
    network = mtcNetwork,
    type = "consistency",
    linearModel = model_type,
    likelihood = like,
    link = link,
    dic = TRUE
  )

  # Run gemtc model object for analysis
  mtcResults <- gemtc::mtc.run(mtcModel)

  mtcRelEffects <- gemtc::relative.effect(mtcResults, t1 = reference_treatment)  #Set reference treatment
  rel_eff_tbl <- gemtc::relative.effect.table(mtcResults)
  sumresults <- summary(mtcRelEffects)
  # a <- paste(model, "effect", sep = " ")   #Create text for random/fixed effect
  sumoverall <- summary(mtcResults)
  dic <- as.data.frame(sumoverall$DIC) # The statistics 'Dbar', 'pD', 'DIC', and 'data points'

  return(
    list(
      mtcResults = mtcResults,
      mtcRelEffects = mtcRelEffects,
      rel_eff_tbl = rel_eff_tbl,
      sumresults = sumresults,
      mtcNetwork = mtcNetwork,
      dic = dic
    )
  )
}

