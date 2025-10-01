#' @title covariate_mcmc
#' @description Does x
#' @param x x
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return NULL
#' @examples {
#' y <- covariate_mcmc(1)
#' }
#' @export
covariate_mcmc <- function(...){
  bayes_mcmc(...)
}

