#' @title baseline_forest
#' @description Produce a forest plot for a baseline risk model using `bnma::network.forest.plot()`
#' @param model model output produced by `baseline_model()`
#' @param title character. Title for the plot. Defaults to `Baseline risk regression analysis`
#' @inheritParams common_params
#' @return NULL
#' @examples {
#' y <- baseline_forest(1)
#' }
#' @export
baseline_forest <- function(model, treatment_df, reference_treatment, title = "Baseline risk regression analysis", logger = NULL){

  if (!inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model()")
    return()
  }

  height <- forest_height(nrow(treatment_df), annotation = TRUE)
  width <- forest_width(14)

  # add Tau sentence somewhere

  svg <- svglite::xmlSVG({
    bnma::network.forest.plot(model$mtcResults, title = title, only.reference.treatment = TRUE)
  },
  width = width,
  height = height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)

}

#' Put the output from a bnma model into the format of a gemtc model, in order to apply CreateTauSentence().
#'
#' @param br_model Output from bnma::network.run.
#' @return List:
#'  - 'sumresults' = List:
#'    - 'summaries' = equivalent of summary(gemtc model)$summaries.
#'    - 'a' = "fixed effect" or "random effect".
FormatForCreateTauSentence <- function(br_model){
  br_summary <- summary(br_model)
  #Rename "sd" (bnma name) to "sd.d" (gemtc name)
  rownames(br_summary$summary.samples$statistics)[rownames(br_summary$summary.samples$statistics) == "sd"] <- "sd.d"
  rownames(br_summary$summary.samples$quantiles)[rownames(br_summary$summary.samples$quantiles) == "sd"] <- "sd.d"
  return(
    list(
      sumresults = list(
        summaries = br_summary$summary.samples
      ),
      a = paste0(br_model$network$type, " effect")
    )
  )
}
