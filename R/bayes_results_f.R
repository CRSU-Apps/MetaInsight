#' Produces a summary of a Bayesian model
#'
#' @param model list. Output produced by `bayes_model()` or `covariate_model()`.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return HTML summary of the model
#' @export
bayes_results <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model() or covariate_model()")
  }

  title <- glue::glue("Results on the {model$sumresults$measure} scale")
  iterations <- glue::glue("Iterations = {model$sumresults$summaries$start}:{model$sumresults$summaries$end}")
  thinning <- glue::glue("Thinning interval = {model$sumresults$summaries$thin}")
  chains <- glue::glue("Number of chains = {model$sumresults$summaries$nchain}")
  sample <- glue::glue("Sample size per chain = {(model$sumresults$summaries$end + 1) - model$sumresults$summaries$start}")
  HTML(paste(title, "", iterations, thinning, chains, sample, sep = "<br/>"))
}

#' @rdname bayes_results
#' @export
covariate_results <- function(...){
  bayes_results(...)
}
