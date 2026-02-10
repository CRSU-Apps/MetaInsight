#' Produce a summary of a Bayesian model
#'
#' @param model list. Output produced by `baseline_model()`, `bayes_model()` or `covariate_model()`.
#' @inheritParams common_params
#' @return HTML summary of the model
#' @export
bayes_results <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model") && !inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  }

  title <- glue::glue("Results on the {model$sumresults$measure} scale")
  iterations <- glue::glue("Iterations = {model$sumresults$summaries$start}:{model$sumresults$summaries$end}")
  thinning <- glue::glue("Thinning interval = {model$sumresults$summaries$thin}")
  chains <- glue::glue("Number of chains = {model$sumresults$summaries$nchain}")
  sample <- glue::glue("Sample size per chain = {(model$sumresults$summaries$end + 1) - model$sumresults$summaries$start}")
  HTML(paste(title, "", iterations, thinning, chains, sample, sep = "<br/>"))
}

#' @rdname bayes_results
#' @param ... Parameters passed to `bayes_results()`
#' @export
covariate_results <- function(...){
  bayes_results(...)
}

#' @rdname bayes_results
#' @param ... Parameters passed to `bayes_results()`
#' @export
baseline_results <- function(...){
  bayes_results(...)
}
