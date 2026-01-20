#' Make a Bayesian forest plot with `gemtc::forest()`
#'
#' @param model list. Object created by `bayes_model()` or `covariate_model()`
#' @param xmin numeric. Minimum x-axis value. Default `NULL` in which case it is calculated internally
#' @param xmax numeric. Maximum x-axis value. Default `NULL` in which case it is calculated internally
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
bayes_forest <- function(model, treatment_df, reference_treatment, xmin = NULL, xmax = NULL, title = "", ranking = FALSE, logger = NULL){

  check_param_classes(c("treatment_df", "reference_treatment", "title", "ranking"),
                      c("data.frame", "character", "character", "logical"), logger)

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model() or covariate_model()")
    return()
  }

  # set default x-axis limits if not supplied and check if they are provided
  if (any(is.null(xmin), is.null(xmax))){
    xlim <- bayes_forest_limits(model, reference_treatment)
    xmin <- ifelse(is.null(xmin), xlim[1], xmin)
    xmax <- ifelse(is.null(xmax), xlim[2], xmax)
  } else {
    check_param_classes(c("xmin", "xmax"), c("numeric", "numeric"), logger)
  }

  if (xmin >= xmax){
    logger |> writeLog(type = "error", "xmin must be less than xmax")
    return()
  }

  height <- forest_height(nrow(treatment_df), title = TRUE, annotation = TRUE)
  width <- forest_width(14 + nchar(reference_treatment))

  svg <- svglite::xmlSVG({
    gemtc::forest(model$mtcRelEffects, digits = 3, xlim = c(xmin, xmax))
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


#' Extract the default x-axis limits using the same method internal to
#' `gemtc::forest()`
#'
#' @param model list. Object created by `bayes_model()` or `covariate_model()`
#' @inheritParams common_params
#' @export
bayes_forest_limits <- function(model, reference_treatment) {

  # whether model is binary
  log.scale <- ifelse(model$outcome_measure == "MD", FALSE, TRUE)

  # Extract reference treatment index
  rel_eff_tbl <- model$rel_eff_tbl
  ref_index <- which(dimnames(rel_eff_tbl)[[1]] == reference_treatment)

  # Extract raw confidence interval bounds
  ci.l_raw <- rel_eff_tbl[ref_index,,1]  # 2.5% quantile
  ci.u_raw <- rel_eff_tbl[ref_index,,3]  # 97.5% quantile

  # Calculate rounded limits
  xmin <- format_xlim(min(ci.l_raw, na.rm=TRUE), "min", log.scale)
  xmax <- format_xlim(max(ci.u_raw, na.rm=TRUE), "max", log.scale)

  # Ensure 0 is included if appropriate (as done in blobbogram)
  xmin <- min(xmin, 0)
  xmax <- max(xmax, 0)

  c(xmin, xmax)
}


