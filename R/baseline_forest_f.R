#' @title baseline_forest
#' @description Produce a forest plot for a baseline risk model using `bnma::network.forest.plot()`
#' @param model model output produced by `baseline_model()`
#' @param title character. Title for the plot. Defaults to `Baseline risk regression analysis`
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
baseline_forest <- function(model, treatment_df, reference_treatment, xmin = NULL, xmax = NULL, title = "Baseline risk regression analysis", ranking = FALSE, logger = NULL){

  check_param_classes(c("treatment_df", "reference_treatment", "title", "ranking"),
                      c("data.frame", "character", "character", "logical"), logger)

  if (!inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model()")
    return()
  }

  height <- forest_height(nrow(treatment_df), annotation = TRUE, title = TRUE)
  width <- forest_width(14)

  median_ci_table <- bnma::relative.effects.table(model$mtcResults, summary_stat = "ci")
  forest_data <- format_baseline_forest(median_ci_table, reference_treatment)

  # set default x-axis limits if not supplied and check if they are provided
  if (any(is.null(xmin), is.null(xmax))){
    xlim <- baseline_forest_limits(forest_data)
    xmin <- ifelse(is.null(xmin), xlim[1], xmin)
    xmax <- ifelse(is.null(xmax), xlim[2], xmax)
  } else {
    check_param_classes(c("xmin", "xmax"), c("numeric", "numeric"), logger)
  }

  if (xmin >= xmax){
    logger |> writeLog(type = "error", "xmin must be less than xmax")
    return()
  }

  label <- paste0("Compared with ", reference_treatment)
  names(label) <- reference_treatment

  ci_label <- switch(model$outcome_measure,
                     "OR" = "Odds Ratio",
                     "RR" = "Risk Ratio",
                     "MD" = "Mean difference")

  svg <- svglite::xmlSVG({
    gemtc::blobbogram(forest_data,
                      id.label = "",
                      group.labels = label,
                      ci.label = paste0(ci_label, " (95% CrI)"),
                      grouped = TRUE,
                      digits = 3,
                      xlim = c(xmin, xmax)
    )
    if (!ranking){
      title(main = title)
      mtext(CreateTauSentence(FormatForCreateTauSentence(model)), padj = 0.5)
      mtext(model$cov_value_sentence, side = 1, padj = -2)
    }

  },
  width = width,
  height = height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)

}

#' Convert output of `bnma::relative.effects.table()` into a suitable format
#' to pass to `gemtc::blobbogram()`
#'
#' @param median_ci_table matrix. Created by `bnma::relative.effects.table()`
#' @inheritParams common_params
#' @export
format_baseline_forest <- function(median_ci_table, reference_treatment) {
  result <- data.frame()

  for (i in 1:nrow(median_ci_table)) {
    for (j in 1:ncol(median_ci_table)) {
      if (i != j && rownames(median_ci_table)[i] == reference_treatment) {
        ci_values <- as.numeric(gsub("\\[|\\]", "", strsplit(median_ci_table[i, j], ",")[[1]]))
        if (!is.na(ci_values[1])) {
          result <- rbind(result, data.frame(
            id = colnames(median_ci_table)[j],
            pe = ci_values[2],
            ci.l = ci_values[1],
            ci.u = ci_values[3],
            style = "normal",
            group = reference_treatment
          ))
        }
      }
    }
  }
  return(result)
}


#' Put the output from a bnma model into the format of a gemtc model, in order to apply CreateTauSentence().
#'
#' @param br_model Output from bnma::network.run.
#' @return List:
#'  - 'sumresults' = List:
#'    - 'summaries' = equivalent of summary(gemtc model)$summaries.
#'    - 'a' = "fixed effect" or "random effect".
FormatForCreateTauSentence <- function(model){
  #Rename "sd" (bnma name) to "sd.d" (gemtc name)
  br_summary <- model$sumresults

  rownames(br_summary$summaries$statistics)[rownames(br_summary$summaries$statistics) == "sd"] <- "sd.d"
  rownames(br_summary$summaries$quantiles)[rownames(br_summary$summaries$quantiles) == "sd"] <- "sd.d"
  return(
    list(
      sumresults = list(
        summaries = br_summary$summaries
      ),
      model_type = model$mtcResults$network$type,
      outcome = model$outcome_measure
    )
  )
}

#' Extract the default x-axis limits using the same method internal to
#' `gemtc::forest()`
#'
#' @param forest_data data.frame. Created by `format_baseline_forest()`
#' @export
baseline_forest_limits <- function(forest_data){

  # forest_data is already transformed
  log.scale <- FALSE

  # Calculate rounded limits
  xmin <- format_xlim(min(forest_data$ci.l, na.rm=TRUE), "min", log.scale)
  xmax <- format_xlim(max(forest_data$ci.u, na.rm=TRUE), "max", log.scale)

  # Ensure 0 is included if appropriate (as done in blobbogram)
  xmin <- min(xmin, 0)
  xmax <- max(xmax, 0)

  c(xmin, xmax)
}
