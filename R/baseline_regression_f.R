#' Generate data required to produce a metaregression plot
#' for a baseline risk model.
#'
#' @param model Output produced by `baseline_model()`
#' @inheritParams common_params
#' @return List containing:
#'  \item{directness}{list. Output from `CalculateDirectness()`}
#'  \item{credible_regions}{list. Output from `CalculateCredibleRegions()`}
#' @export
baseline_regression <- function(model, configured_data, async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("model", "configured_data"),
                            c("baseline_model", "configured_data"), NULL)){
      return()
    }
  }

  if (FindDataShape(configured_data$connected_data) == "wide") {
    connected_data <- WideToLong(configured_data$connected_data, outcome = outcome)
  }

  reference_outcome <- GetReferenceOutcome(configured_data$connected_data,
                                           configured_data$treatments,
                                           configured_data$outcome,
                                           "Imputed",
                                           model)

  data_with_covariates_removed <- dplyr::select(configured_data$connected_data, !dplyr::starts_with("covar."))
  data_with_covariate <- merge(data_with_covariates_removed,
                               data.frame(Study = names(reference_outcome),
                                          covar.baseline_risk = reference_outcome,
                                          row.names = NULL),
                               by = "Study"
  )

  directness <- CalculateDirectness(
    data = data_with_covariate,
    covariate_title = "covar.baseline_risk",
    treatment_ids = configured_data$treatments,
    outcome = configured_data$outcome,
    outcome_measure = configured_data$outcome_measure,
    effects_type = configured_data$effects)

  credible_regions <- CalculateCredibleRegionsBnma(model)

  output <- list(
    directness = directness,
    credible_regions = credible_regions)

  class(output) <- "regression_data"
  output
}


#' Calculate the credible regions within direct evidence for the baseline risk model.
#'
#' @param model_output Return from `BaselineRiskModelOutput()`.
#'
#' @return list of credible region objects and credible interval objects.
#' Regions cover treatments with a non-zero covariate range of direct contributions,
#' intervals cover treatments with a single covariate value from direct contributions.
#' Any treatment with no direct contributions will not be present in either list.
#' Each is a list of data frames for each treatment name. Each data frame contains 3 columns:
#' - cov_value: The covariate value at which the credible region is calculated.
#' - lower: the 2.5% quantile.
#' - upper: the 97.5% quantile.
#' Each data frame in "regions" contains 11 rows creating a 10-polygon region.
#' Each data frame in "intervals" contains a single row at the covariate value of that single contribution.
CalculateCredibleRegionsBnma <- function(model_output) {

  mtc_results <- model_output$mtcResults
  treatments <- mtc_results$network$Treat.order

  credible_regions <- list()
  credible_intervals <- list()

  for (treatment_name in model_output$comparator_names) {
    parameter_name <- glue::glue("d[{which(treatment_name == unname(treatments))}]")
    cov_min <- model_output$covariate_min[treatment_name]
    cov_max <- model_output$covariate_max[treatment_name]

    if (is.na(cov_min)) {
      credible_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      credible_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else if (cov_min == cov_max) {
      interval <- .FindCredibleIntervalBnma(
        mtc_results = mtc_results,
        cov_value = cov_min,
        parameter_name = parameter_name
      )
      df <- data.frame(
        cov_value = cov_min,
        lower = interval["2.5%"],
        upper = interval["97.5%"]
      )

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      credible_intervals[[treatment_name]] <- df
      credible_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else {
      df <- data.frame()
      for (cov_value in seq(from = cov_min, to = cov_max, length.out = 11)) {
        interval <- .FindCredibleIntervalBnma(
          mtc_results = mtc_results,
          cov_value = cov_value,
          parameter_name = parameter_name
        )
        df <- rbind(
          df,
          data.frame(
            cov_value = cov_value,
            lower = interval["2.5%"],
            upper = interval["97.5%"]
          )
        )
      }

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      credible_regions[[treatment_name]] <- df
      credible_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    }
  }

  return(
    list(
      regions = credible_regions,
      intervals = credible_intervals
    )
  )
}


#' Find the credible interval at a given covariate value.
#'
#' @param mtc_results Meta-analysis object from which to find credible interval.
#' @param cov_value Covariate value at which to find the credible interval.
#' @param parameter_name Name of the parameter for which to get the credible interval.
#'
#' @return Named vector of "2.5%" and "97.5" quantiles.
.FindCredibleIntervalBnma <- function(mtc_results, cov_value, parameter_name) {
  rel_eff <- BnmaRelativeEffects(
    model = mtc_results,
    covariate_value = cov_value
  )
  return(
    rel_eff[parameter_name, c("2.5%", "97.5%")]
  )
}



