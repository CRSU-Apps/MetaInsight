#' @title baseline_forest
#' @description Does x
#' @param x x
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return NULL
#' @examples {
#' y <- baseline_forest(1)
#' }
#' @export
baseline_forest <- function(...){
  bayes_forest(...)
}

