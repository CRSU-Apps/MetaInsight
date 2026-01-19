#' @title baseline_comparison
#' Creates a table of comparisons of all treatment pairs for baseline risk models
#'
#' @param model list. Object created by `baseline_model()`
#' @inheritParams common_params
#' @return Relative effects table created by `gemtc::relative.effect.table()`
#' @export
baseline_comparison <- function(model, logger = NULL){

  if (!inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model()")
  }

  median_ci_table <- bnma::relative.effects.table(model$mtcResults, summary_stat = "ci")
  BaselineRiskRelativeEffectsTable(median_ci_table)
}
