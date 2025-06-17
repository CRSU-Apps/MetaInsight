#' Produces a summary of the fitted Bayesian model
#'
#' @param model Various model output created by bayes_model().
#' @return HTML summary of the model
#' @export
bayes_results <- function(model, logger = NULL){
  title <- glue::glue("Results on the {model$sumresults$measure} scale")
  iterations <- glue::glue("Iterations = {model$sumresults$summaries$start}:{model$sumresults$summaries$end}")
  thinning <- glue::glue("Thinning interval = {model$sumresults$summaries$thin}")
  chains <- glue::glue("Number of chains = {model$sumresults$summaries$nchain}")
  sample <- glue::glue("Sample size per chain = {(model$sumresults$summaries$end + 1) - model$sumresults$summaries$start}")
  HTML(paste(title, "", iterations, thinning, chains, sample, sep = "<br/>"))
}
