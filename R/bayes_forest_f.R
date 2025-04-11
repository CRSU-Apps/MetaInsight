#' Make a Bayesian forest plot
#'
#' @param model description
#' @export
bayes_forest <- function(bayes_model){
  return(gemtc::forest(bayes_model$mtcRelEffects, digits = 3))#, xlim = c(bayesmin, bayesmax)))
}
