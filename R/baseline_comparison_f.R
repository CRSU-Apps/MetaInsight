#' @title Compare treatment pairs
#' @description Produce a table of comparisons of all treatment pairs for baseline risk
#' models using `bnma::relative.effects.table()`
#'
#' @param model list. Object produced by `baseline_model()`
#' @inheritParams common_params
#' @return Relative effects table
#' @examples
#' \donttest{
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' fitted_baseline_model <- baseline_model(configured_data = configured_data,
#'                                         regressor_type = "shared")
#'
#' baseline_comparison(model = fitted_baseline_model)
#' }
#' @export
baseline_comparison <- function(model, logger = NULL){

  if (!inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model()")
  }

  median_ci_table <- bnma::relative.effects.table(model$mtcResults, summary_stat = "ci")
  BaselineRiskRelativeEffectsTable(median_ci_table)
}
