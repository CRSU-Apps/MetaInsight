#' Make a Bayesian forest plot
#'
#' @param bayes_model list Objected created by bayes_model()
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return None - produces a base plot
#' @export
bayes_forest <- function(bayes_model, logger = NULL){

  if (!inherits(bayes_model, "list") || !"mtcRelEffects" %in% names(bayes_model) || !inherits(bayes_model$mtcRelEffects, "mtc.result")){
    logger |> writeLog(type = "error", "bayes_model must be an object created by bayes_model()")
  }

  gemtc::forest(bayes_model$mtcRelEffects, digits = 3)
}
