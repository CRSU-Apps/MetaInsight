#' @title Analysis of heterogeneity
#' @description Produce a table containing heterogeneity statistics for Bayesian models using `gemtc::mtc.anohe()`
#'
#' @param model list. Object created by `bayes_model()`
#' @inheritParams common_params
#' @return Data frame of pairwise and global heterogeneity statistics, with columns `t1`, `t2`, `i2.pair`, `i2.cons`, and `incons.p`.
#' 
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' bayes_anohe(model = fitted_bayes_model)
#'
#' @export
bayes_anohe <- function(model, logger = NULL){
  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model()")
    return()
  }
  anohe_summary <- summary(gemtc::mtc.anohe(model$mtcNetwork))
  anohe_global <- data.frame(
    t1 = "Global",
    t2 = "-",
    i2.pair = anohe_summary[["isquared.glob"]][["i2.pair"]] / 100,
    i2.cons = anohe_summary[["isquared.glob"]][["i2.cons"]] / 100,
    incons.p = NA
  )
  anohe_table <- rbind(anohe_summary[["isquared.comp"]], anohe_global)
  return(anohe_table)
}

