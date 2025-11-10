#' Make a Bayesian forest plot
#'
#' @param model list. Object created by `bayes_model()` or `covariate_model()`
#' @param title character. Title for the plot. Default is no title
#' @param ranking logical. Whether the function is being used in `bayes_ranking`
#' @inheritParams common_params
#'
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @export
bayes_forest <- function(model, treatment_df, reference_treatment, title = "", ranking = FALSE, logger = NULL){

  check_param_classes(c("treatment_df", "reference_treatment", "title", "ranking"),
                      c("data.frame", "character", "character", "logical"), logger)

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model() or covariate_model()")
    return()
  }

  height <- forest_height(nrow(treatment_df), title = TRUE, annotation = TRUE)
  width <- forest_width(14 + nchar(reference_treatment))

  svg <- svglite::xmlSVG({
    gemtc::forest(model$mtcRelEffects, digits = 3)
    if (!ranking){
      title(main = title)
      mtext(CreateTauSentence(model), padj = 0.5)
      if (inherits(model, "covariate_model")){
        mtext(model$cov_value_sentence, side = 1, padj = -2)
      }
    }
    },
    width = width,
    height = height,
    web_fonts = list(
      arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)
}

#' @rdname bayes_forest
#' @param ... Parameters passed to `bayes_forest()`
#' @export
covariate_forest <- function(...){
  bayes_forest(...)
}
