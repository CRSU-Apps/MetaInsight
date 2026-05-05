#' @title Summarise a Bayesian model
#' @description Produce a table summarising Bayesian models
#'
#' @param model list. Output produced by `baseline_model()`, `bayes_model()` or `covariate_model()`.
#' @inheritParams common_params
#' @return HTML summary of the model
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_adapt and n_iter are set low to run quickly, but should be left as the
#' # default values in real use
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' bayes_results(fitted_bayes_model)
#' @export
bayes_results <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model") && !inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  }

  if (inherits(model, "bayes_model")) {
    if (model$mtcResults$model$type == "consistency") {
      title <- glue::glue("The results are on the {model$sumresults$measure} scale.")
    }
    if (model$mtcResults$model$type == "regression") {
      covariate_parameter_text <- switch(
        model$mtcResults$model$regressor$coefficient,
        "shared" = "B and B_unscaled",
        "exchangeable" = "B/beta/reg.sd and B_unscaled/beta_unscaled/reg.sd_unscaled",
        "unrelated" = "beta and beta_unscaled"
      )
      title <- glue::glue("The results are on the {model$sumresults$measure} scale. The relative treatment effect (d) parameters are the relative treatment effects at the selected covariate value ({model$mtcRelEffects$covariate |> round(digits = 3)}). The two sets of covariate parameters ({covariate_parameter_text}) correspond to scaled and unscaled covariate values respectively. The scaled values are the values used when the model is fit. They are divided by a scaling factor, in this case {round(model$mtcResults$model$regressor$scale, digits = 3)}, so that they have standard deviation 0.5.")
    }
  } else if (inherits(model, "baseline_model")) {
    title <- glue::glue("The results are on the {model$sumresults$measure} scale. The covariate values are centred to aid with model fit. The relative treatment effect (d) parameters are the relative treatment effects at the centring value ({model$mtcResults$network$mx_bl |> round(digits = 3)}).")
  }
  iterations <- glue::glue("Iterations = {model$sumresults$summaries$start}:{model$sumresults$summaries$end}")
  thinning <- glue::glue("Thinning interval = {model$sumresults$summaries$thin}")
  chains <- glue::glue("Number of chains = {model$sumresults$summaries$nchain}")
  sample <- glue::glue("Sample size per chain = {(model$sumresults$summaries$end + 1) - model$sumresults$summaries$start}")
  HTML(paste(title, "", iterations, thinning, chains, sample, sep = "<br/>"))
}

#' @rdname bayes_results
#' @param ... Parameters passed to `bayes_results()`
#' @export
covariate_results <- function(...){
  bayes_results(...)
}

#' @rdname bayes_results
#' @param ... Parameters passed to `bayes_results()`
#' @export
baseline_results <- function(...){
  bayes_results(...)
}
