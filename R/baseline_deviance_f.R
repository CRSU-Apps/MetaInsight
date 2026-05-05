#' @title Produce deviance plots for baseline risk models
#' @description Produce deviance plotly plots for baseline risk models. Unlike for
#' `bayes_model` output, only stem and leverage plots are produced.
#' @param model Output model produced by `baseline_model()`
#' @inheritParams common_params
#' @return list containing:
#'  \item{deviance_mtc}{equivalent summary to that produced by `gemtc::mtc.deviance()`}
#'  \item{stem_plot}{plotly object}
#'  \item{lev_plot}{plotly object}
#'
#' @examples
#' \donttest{
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_iter, max_iter and check_iter are set low to run quickly, but should
#' # be left as the default values in real use
#'
#' fitted_baseline_model <- baseline_model(configured_data = configured_data,
#'                                         regressor_type = "shared",
#'                                         n_iter = 120,
#'                                         max_iter = 120,
#'                                         check_iter = 10)
#'
#' baseline_deviance(model = fitted_baseline_model)
#' }
#' @export
baseline_deviance <- function(model, async = FALSE){

  if (!inherits(model, "baseline_model")){
    return(async |> asyncLog(type = "error", "model must be an object created by baseline_model()"))
  }

  deviance <- list(dev.ab = model$mtcResults$deviance$dev_arm,
                   fit.ab = model$mtcResults$deviance$devtilda_arm,
                   dev.re = NULL,
                   fit.re = NULL,
                   nd.ab = model$mtcResults$network$na,
                   nd.re = NULL
  )

  # for consistency
  class(deviance) <- "mtc.deviance"

  list(deviance_mtc = deviance,
       stem_plot = stem_plot(deviance),
       lev_plot = lev_plot(deviance))
}

