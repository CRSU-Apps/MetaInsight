#' Creates a table of comparisons of all treatment pairs for Bayesian models
#'
#' @param model list. Object created by `bayes_model()` or `covariate_model()`
#' @inheritParams common_params
#' @return Relative effects table created by `gemtc::relative.effect.table()`
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
#' @export
covariate_comparison <- function(...){
  bayes_compare(...)
}
