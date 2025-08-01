#' Make a Bayesian forest plot
#'
#' @param model list Objected created by bayes_model()
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return None - produces a base plot
#' @export
bayes_forest <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model()")
    return()
  }
  gemtc::forest(model$mtcRelEffects, digits = 3)
}
