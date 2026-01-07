#' @title baseline_regression
#' @description Generate data required to produce a metaregression plot
#' for a baseline risk model.
#' @param model list. model output produced by `baseline_model()`
#' @param covariate_title character. Required for consistency with `covariate_regression()`.
#' Do not change from the default `covar.baseline_risk`
#' @inheritParams common_params
#' @return List containing:
#'  \item{directness}{list. Output from `CalculateDirectness()`}
#'  \item{credible_regions}{list. Output from `CalculateCredibleRegions()`}
#' @export
baseline_regression <- function(model,
                                 connected_data,
                                 covariate_title = "covar.baseline_risk",
                                 treatment_df,
                                 outcome,
                                 outcome_measure,
                                 model_type,
                                 async = FALSE){

  if (FindDataShape(connected_data) == "wide") {
    connected_data <- WideToLong(connected_data, outcome = outcome)
  }

  reference_outcome <- GetReferenceOutcome(connected_data, treatment_df, outcome, "Imputed", model)

  data_with_covariates_removed <- dplyr::select(connected_data, !dplyr::starts_with("covar."))
  data_with_covariate <- merge(data_with_covariates_removed,
                               data.frame(Study = names(reference_outcome),
                                          covar.baseline_risk = reference_outcome,
                                          row.names = NULL),
                               by = "Study"
  )

  directness <- CalculateDirectness(
    data = data_with_covariate,
    covariate_title = covariate_title,
    treatment_ids = treatment_df,
    outcome = outcome,
    outcome_measure = outcome_measure,
    effects_type = model_type)

  credible_regions <- CalculateCredibleRegionsBnma(model)

  list(directness = directness,
       credible_regions = credible_regions)

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

#' Puts a relative effects table from bnma into gemtc format
#'
#' @param median_ci_table Output from bnma::relative.effects.table(, summary_stat = "ci")
#' @return A relative effects table in the same format as from gemtc.
BaselineRiskRelativeEffectsTable <- function(median_ci_table) {
  #Entries in the input table are in the form "[lower_ci,median,upper_ci]" (no spaces)

  #The dimensions of the (square) table
  dim_median <- nrow(median_ci_table)
  #Create matrices to store the lower_ci, median and upper_ci separately
  lower_ci <- matrix(nrow = dim_median, ncol = dim_median)
  median_br <- matrix(nrow = dim_median, ncol = dim_median)
  upper_ci <- matrix(nrow = dim_median, ncol = dim_median)

  for (row in 1:dim_median) {
    for (col in 1:dim_median) {
      #Extract lower_ci, median and upper_ci
      interval <- round(
        as.numeric(
          stringr::str_extract_all(
            string = median_ci_table[row, col],
            pattern = "[-0-9\\.]+")[[1]]
        ),
        digits = 2
      )
      lower_ci[row, col] <- interval[1]
      median_br[row, col] <- interval[2]
      upper_ci[row, col] <- interval[3]
    }
  }

  #Paste into the format "median (lower_ci, upper_ci)"
  median_ci_table_new <- matrix(paste0(median_br, " (", lower_ci, ", ", upper_ci, ")"), nrow = dim_median)
  diag(median_ci_table_new) <- rownames(median_ci_table)
  rownames(median_ci_table_new) <- rownames(median_ci_table)
  colnames(median_ci_table_new) <- colnames(median_ci_table)

  return(median_ci_table_new)
}

