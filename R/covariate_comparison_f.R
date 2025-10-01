#' @title covariate_comparison
#' @description Does x
#' @param x x
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return NULL
#' @examples {
#' y <- covariate_comparison(1)
#' }
#' @export
covariate_comparison <- function(...){
  bayes_compare(...)
}

