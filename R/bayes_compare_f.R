#' @title Compare treatment pairs
#' @description Produce a table of comparisons of all treatment pairs for Bayesian
#' models using `gemtc::relative.effect.table()`
#'
#' @param model list. Object created by `bayes_model()` or `covariate_model()`
#' @inheritParams common_params
#' @return Relative effects table
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data)
#' bayes_compare(model = fitted_bayes_model)
#' @export
bayes_compare <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model() or covariate_model()")
    return()
  }

  if (model$outcome_measure %in% c("OR", "RR")) {
    return(as.data.frame(round(exp(model$rel_eff_tbl), digits = 2)))
  } else if (model$outcome_measure %in% c("RD", "MD", "SMD")) {
    return(as.data.frame(round(model$rel_eff_tbl, digits = 2)))
  }
}

#' @rdname bayes_compare
#' @param ... Parameters passed to `bayes_compare()`
#' @export
covariate_compare <- function(...){
  bayes_compare(...)
}
