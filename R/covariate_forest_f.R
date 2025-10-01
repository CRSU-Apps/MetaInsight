#' @title covariate_forest
#' @description Produce a forest plot from the covariate model. This is
#' a wrapper for `bayes_forest()` - see that for documentation.
#' @param ... parameters passed to `bayes_forest()`
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @export
covariate_forest <- function(...){
  bayes_forest(...)
}

