#' @title covariate_regression
#' @description Calculate data from a covariate model required to produce a
#' metaregression plot
#' @param model list. Output created by `covariate_model()`
#' @param covariate_column character. Name of the column containing the covariate data
#' @inheritParams common_params
#' @return List containing:
#'  \item{directness}{list. Output from `CalculateDirectness()`}
#'  \item{credible_regions}{list. Output from `CalculateCredibleRegions()`}
#' @export
covariate_regression <- function(model,
                                 connected_data,
                                 covariate_column,
                                 treatment_df,
                                 outcome,
                                 outcome_measure,
                                 model_type,
                                 async = FALSE){

  # need to look this up
  # cov_parameters <- model_output$mtcResults$model$regressor$coefficient

  if (FindDataShape(connected_data) == "wide") {
    connected_data <- WideToLong(connected_data, outcome = outcome)
  }
  directness <- CalculateDirectness(
    data = connected_data,
    covariate_title = covariate_column,
    treatment_ids = treatment_df,
    outcome = outcome,
    outcome_measure = outcome_measure,
    effects_type = model_type)

  credible_regions <- CalculateCredibleRegions(model)

  list(directness = directness,
       credible_regions = credible_regions)
}


#' Determine whether comparisons contribute to relative treatment effects directly, indirectly, or not at all.
#'
#' @param data Input data in long format.
#' @param covariate_title Title of covariate column in data. Enter NULL if there is no covariate.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR", "RR" or "RD".
#' @param effects_type "fixed" or "random".
#' @return List of contributions:
#' - "direct"
#'   - Matrix containing TRUE, FALSE, or NA corresponding to direct, indirect, or no contribution. Rows are studies, columns are treatments
#' - "relative_effect"
#'   - Matrix of relative effects of treatments compared to the reference. Rows are studies, columns are treatments
#' - "covariate_value"
#'   - Vector of covariate values from the studies.
CalculateDirectness <- function(
    data,
    covariate_title,
    treatment_ids,
    outcome,
    outcome_measure,
    effects_type) {

  if (outcome == "Binary") {
    d0 <- meta::pairwise(treat = data$T, event = data$R, studlab = data$Study, n = data$N, sm = outcome_measure, data = data)
  } else if (outcome == "Continuous") {
    d0 <- meta::pairwise(treat = data$T, mean = data$Mean, sd = data$SD, studlab = data$Study, n = data$N, sm = outcome_measure, data = data)
  } else {
    stop(glue::glue("Outcome type '{outcome}' is not supported. Please use 'Binary' or 'Continuous'"))
  }

  #Switch the treatment effects to match the rest of the app.
  d0$TE <- -d0$TE

  reference_index <- 1
  reference <- treatment_ids$Label[treatment_ids$Number == reference_index]
  treatments <- treatment_ids$Label[treatment_ids$Label != reference]
  studies <- unique(data$Study)

  is_direct <- matrix(FALSE,
                      nrow = length(studies),
                      ncol = length(treatments),
                      dimnames = list(
                        studies,
                        treatments
                      )
  )

  is_indirect <- matrix(FALSE,
                        nrow = length(studies),
                        ncol = length(treatments),
                        dimnames = list(
                          studies,
                          treatments
                        )
  )

  #Create the network
  network <- CreateGraph(data = data)

  #Populate 'is_direct'  and 'is_indirect'
  for (study in studies) {
    study_treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids, study = study)
    study_treatment_index <- match(study_treatments, treatment_ids$Label)
    for (treatment in treatments) {
      #If the study contains both the reference and this treatment then it makes a direct contribution
      if (all(c(reference, treatment) %in% study_treatments)) {
        is_direct[study, treatment] <- TRUE
      }

      treatment_number <- treatment_ids$Number[treatment_ids$Label == treatment]
      #Find all paths between the reference and this treatment
      paths <- igraph::all_simple_paths(graph = network,
                                        from = 1,
                                        to = treatment_number)
      if (length(paths) != 0) {
        #If there are paths, check there is at least one that (1) is indirect (2) contains at least two treatments from this study (3) contains at least one intermediate node (not the start or end node). If so then this study contributes to an indirect path between the reference and this treatment.
        for (path in paths) {
          if (length(path) > 2
              && sum(study_treatment_index %in% path) >= 2
              && any(study_treatment_index %in% path[-c(1, length(path))])) {
            is_indirect[study, treatment] <- TRUE
            break
          }
        }
      }
    }
  }

  relative_effects <- matrix(
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )

  #Populate 'relative_effects'
  for (study in studies) {
    for (treatment in treatments) {
      treatment_index <- treatment_ids$Number[treatment_ids$Label == treatment]
      treatment_effect <- d0$TE[d0$treat1 == reference_index & d0$treat2 == treatment_index & d0$Study == study]

      if (length(treatment_effect) != 0) {
        relative_effects[study, treatment] <- treatment_effect
      }
    }
  }

  covariate_values <- data[[covariate_title]][match(studies, data$Study)]
  names(covariate_values) <- studies

  return(
    list(
      is_direct = is_direct,
      is_indirect = is_indirect,
      relative_effect = relative_effects,
      covariate_value = covariate_values
    )
  )
}


#' Calculate the credible regions within direct evidence for the regression model.
#'
#' @param model_output Return from `CovariateModelOutput()`.
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
CalculateCredibleRegions <- function(model_output) {
  mtc_results <- model_output$mtcResults
  reference_treatment <- model_output$reference_treatment

  credible_regions <- list()
  credible_intervals <- list()

  for (treatment_name in model_output$comparator_names) {
    parameter_name <- glue::glue("d.{reference_treatment}.{treatment_name}")
    cov_min <- model_output$covariate_min[treatment_name]
    cov_max <- model_output$covariate_max[treatment_name]

    if (is.na(cov_min)) {

      credible_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      credible_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)

    } else if (cov_min == cov_max) {

      interval <- .FindCredibleInterval(mtc_results, reference_treatment, cov_min, parameter_name)
      df <- data.frame(cov_value = cov_min, lower = interval["2.5%"], upper = interval["97.5%"])

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      credible_intervals[[treatment_name]] <- df
      credible_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)

    } else {

      df <- data.frame()

      #Set the covariate values along the x-axis
      if (model_output$mtcResults$model$regressor$type == "continuous") {
        cov_value_sequence <- seq(from = cov_min, to = cov_max, length.out = 11)
      } else if (model_output$mtcResults$model$regressor$type == "binary") {
        cov_value_sequence <- 0:1
      }

      for (cov_value in cov_value_sequence) {
        interval <- .FindCredibleInterval(mtc_results, reference_treatment, cov_value, parameter_name)
        df <- rbind(
          df,
          data.frame(cov_value = cov_value, lower = interval["2.5%"], upper = interval["97.5%"])
        )
      }

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      if (model_output$mtcResults$model$regressor$type == "continuous") {
        credible_regions[[treatment_name]] <- df
        credible_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      } else if (model_output$mtcResults$model$regressor$type == "binary") {
        credible_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
        credible_intervals[[treatment_name]] <- df
      }
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
#' @param reference_treatment Name of reference treatment.
#' @param cov_value Covariate value at which to find the credible interval.
#' @param parameter_name Name of the parameter for which to get the credible interval.
#'
#' @return Named vector of "2.5%" and "97.5" quantiles.
.FindCredibleInterval <- function(mtc_results, reference_treatment, cov_value, parameter_name) {
  rel_eff <- gemtc::relative.effect(mtc_results, reference_treatment, covariate = cov_value)
  rel_eff_summary <- summary(rel_eff)
  return(rel_eff_summary$summaries$quantiles[parameter_name, c("2.5%", "97.5%")])
}

