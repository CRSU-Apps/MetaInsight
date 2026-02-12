#' Prepare study contributions into a format that CINeMA can read.
#' This is a named list of lists. The top level list has item named "<TREATMENT_N>:<TREATMENT_M>"
#' where "<TREATMENT_N>" and "<TREATMENT_M>" are the names of 2 treatments. There is an item
#' for every possible comparison in the network.
#' Each item in the second level is a named list of numerical values for the contribution of each
#' study towards the comparison. The name of each item is the name of the relevant study, and
#' every study is included in the list. The values sum to 1.
#' 
#' @param study_contributions The contribution matrix calculated by {netmeta}.
#' @return A List with the described structure.
.PrepareStudyContibutionsForCinema <- function(study_contributions, data) {
  prepared_contributions <- sapply(
    simplify = FALSE,
    unique(study_contributions$comparison),
    function(comparison) {
      studies_for_comparison <- study_contributions$study[study_contributions$comparison == comparison]
      study_indices <- unlist(
        lapply(
          studies_for_comparison,
          function(study) {
            return(data$StudyID[data$Study == study][1])
          }
        )
      ) |>
        as.character()
      
      return(
        sapply(
          simplify = FALSE,
          study_indices,
          function(study_id) {
            study <- data$Study[data$StudyID == study_id]
            return(
              jsonlite::unbox(
                100 * study_contributions$contribution[study_contributions$comparison == comparison & study_contributions$study == study]
              )
            )
          }
        )
      )
    }
  )
  
  return(prepared_contributions)
}

#' Prepare data into a format that CINeMA can read.
#' This is an unnamed list of lists. The top level list contains an item for each study arm,
#' as in long format.
#' Each item in the second level is a named list of study arm properties:
#' - "study" Study name
#' - "id" Unique study ID
#' - "t" Name of treatment
#' - "n" Number of participants
#' - "r" Number of events (Binary outcome)
#' - "mean" Mean treatment effect (Continuous outcome)
#' - "sd" Standard deviation of treatment effect (Continuous outcome)
#' - "indirectness" Indirectness of study
#' - "rob" Risk of bias of study
#' 
#' @param data The dataframe used in MetaInsight.
#' @param treatment_ids  Data frame containing treatment names (Label), original tratment names (RawLabel) and IDs (Number).
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'.
#' @return A List with the described structure.
.PrepareDataForCinema <- function(data, treatment_ids, outcome_type) {
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome_type)
  } else {
    long_data <- data
  }
  
  long_data <- ReinstateTreatmentIds(long_data, treatment_ids, raw_label = TRUE)
  
  prepared_data <- lapply(
    1:nrow(long_data),
    function(index) {
      item = list()
      
      item$study <- jsonlite::unbox(long_data[index, "RawStudy"])
      item$id <- jsonlite::unbox(long_data[index, "StudyID"])
      item$t <- jsonlite::unbox(long_data[index, "T"])
      item$n <- jsonlite::unbox(long_data[index, "N"])
      item$rob <- jsonlite::unbox(long_data[index, "RoB"])
      item$indirectness <- jsonlite::unbox(long_data[index, "Indirectness"])
      
      if (outcome_type == "Binary") {
        item$r <- jsonlite::unbox(long_data[index, "R"])
      } else if (outcome_type == "Continuous") {
        item$mean <- jsonlite::unbox(long_data[index, "Mean"])
        item$sd <- jsonlite::unbox(long_data[index, "SD"])
      } else {
        stop(glue::glue("Outcome type '{outcome_type}' not supported"))
      }
      
      return(item)
    }
  )
  
  return(prepared_data)
}

#' Prepare analysis into a format that CINeMA can read.
#' This is a named list of lists. The top level list contains items:
#' - "hatmatrix" Hat matrix from {netmeta}
#'   - "colNames" Vector of column names in form "<TREATMENT_N>:<TREATMENT_M>"
#'   - "H" Matrix of matrices
#'   - "model" Type of model: "fixed" or "random"
#'   - "rowNames" Vector of row names in form "<TREATMENT_N>:<TREATMENT_M>"
#'   - "sm" Outcome measure, one of: ["OR", "RR", "RD", "MD", "SMD"]
#' - "studycontributions" Output from `.PrepareStudyContibutionsForCinema()`
#' 
#' @param contributions Contributions from {netmeta}.
#' @param model_type Type of model: "fixed" or "random".
#' @param outcome_measure Outcome measure, one of: ["OR", "RR", "RD", "MD", "SMD"].
#' @param gemtc_results Output from gemtc::mtc.run, as returned in the 'mtcResults' list element from baye(). If this parameter is NULL then the frequentist analysis results found in 'contributions' are used exclusively. If it is not NULL then the Bayesian analysis results contained in this parameter are used as well. Defaults to NULL.
#' @return A List with the described structure.
.PrepareAnalysisForCinema <- function(contributions, model_type, outcome_measure, gemtc_results = NULL) {
  hat_matrix = netmeta::hatmatrix(x = contributions$x, method = "Davies", type = "long")
  if (model_type == "fixed") {
    h <- hat_matrix$common
    study_contributions <- contributions$study.common
  } else if (model_type == "random") {
    h <- hat_matrix$random
    study_contributions <- contributions$study.random
  } else {
    stop(glue::glue("Model type'{model_type}' not supported"))
  }
  
  nma_col_names = c(
    "Direct",
    "DirectL",
    "DirectU",
    "Indirect",
    "IndirectL",
    "IndirectU",
    "SideIF",
    "SideIFlower",
    "SideIFupper",
    "SideZ",
    "SidePvalue",
    "PropDir",
    "NMA treatment effect",
    "se treat effect",
    "lower CI",
    "upper CI",
    "lower PrI",
    "upper PrI",
    "PropDirNetmeta",
    "_row"
  )
  
  prepared_hat_matrix <- list(
    colNames = colnames(h),
    colNamesNMAresults = nma_col_names,
    H = h,
    model = jsonlite::unbox(model_type),
    NMAresults = .PrepareComparisonsForCinema(contributions, model_type, outcome_measure, gemtc_results),
    rowNames = rownames(h),
    rowNamesNMAresults = rownames(h),
    sm = jsonlite::unbox(outcome_measure)
  )
  
  prepared_analysis <- list(
    contributionMatrices = list(
      list(
        hatmatrix = prepared_hat_matrix,
        studycontributions = .PrepareStudyContibutionsForCinema(study_contributions, contributions$x$data)
      )
    )
  )
  
  return(prepared_analysis)
}

#' Prepare project into a format that CINeMA can read.
#' This is a named list of lists. The top level list contains items:
#' - "project" Information for CINeMA project
#'   - "CM" Contribution matrices
#'     - "contributionMatrices" output from `.PrepareAnalysisForCinema()`
#'   - "format" Data format. Always "long"
#'   - "type" Outcome type. Either "binary" or "continuous"
#'   - "Studies" Study data
#'     - "long" Output from `.PrepareDataForCinema()`
#' 
#' @param data The dataframe used in MetaInsight.
#' @param treatment_ids  Data frame containing treatment names (Label), original tratment names (RawLabel) and IDs (Number).
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'.
#' @param contributions Contributions from {netmeta}.
#' @param model_type Type of model: "fixed" or "random".
#' @param outcome_measure Outcome measure, one of: ["OR", "RR", "RD", "MD", "SMD"].
#' @param gemtc_results Output from gemtc::mtc.run, as returned in the 'mtcResults' list element from baye(). If this parameter is NULL then the frequentist analysis results found in 'contributions' are used exclusively. If it is not NULL then the Bayesian analysis results contained in this parameter are used as well. Defaults to NULL.
#' @return A List with the described structure.
.PrepareProjectForCinema <- function(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure, gemtc_results = NULL) {
  prepared_data <- .PrepareDataForCinema(data, treatment_ids, outcome_type)
  prepared_analysis <- .PrepareAnalysisForCinema(contributions, model_type, outcome_measure, gemtc_results)
  
  prepared_project = list(
    project = list(
      CM = prepared_analysis,
      format = jsonlite::unbox("long"),
      type = jsonlite::unbox(tolower(outcome_type)),
      studies = list(
        long = prepared_data
      )
    )
  )
  
  return(prepared_project)
}

#' Prepare project into a JSON format that CINeMA can read.
#' This is a named list of lists. The top level list contains items:
#' - "project" Information for CINeMA project
#'   - "CM" Contribution matrices
#'     - "contributionMatrices" output from `.PrepareAnalysisForCinema()`
#'   - "format" Data format. Always "long"
#'   - "type" Outcome type. Either "binary" or "continuous"
#'   - "Studies" Study data
#'     - "long" Output from `.PrepareDataForCinema()`
#' 
#' @param data The dataframe used in MetaInsight.
#' @param treatment_ids  Data frame containing treatment names (Label), original tratment names (RawLabel) and IDs (Number).
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'.
#' @param contributions Contributions from {netmeta}.
#' @param model_type Type of model: "fixed" or "random".
#' @param outcome_measure Outcome measure, one of: ["OR", "RR", "RD", "MD", "SMD"].
#' @param gemtc_results Output from gemtc::mtc.run, as returned in the 'mtcResults' list element from baye(). If this parameter is NULL then the frequentist analysis results found in 'contributions' are used exclusively. If it is not NULL then the Bayesian analysis results contained in this parameter are used as well. Defaults to NULL.
#' @return JSON string with the described structure.
GenerateCinemaJson <- function(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure, gemtc_results = NULL) {
  prepared_project <- .PrepareProjectForCinema(
    data,
    treatment_ids,
    outcome_type,
    contributions,
    model_type,
    outcome_measure,
    gemtc_results
  )
  return(jsonlite::toJSON(prepared_project, pretty = TRUE, na = "null"))
}



#' Creates a list, each element of which is a named list containing the results of a comparison. In the outer list there is one element per comparison. Each inner list contains results including treatment effects, standard errors, confidence intervals, predictions intervals, and p-values, for NMA effects, direct and indirect effects, and SIDE inconsistency factors. If there is no such data (e.g. if there is no indirect evidence) then the list element does not appear.
#'
#' @param contributions Contributions from {netmeta}.
#' @param model_type Type of model: "fixed" or "random".
#' @param outcome_measure Outcome measure, one of: ["OR", "RR", "RD", "MD", "SMD"].
#' @param gemtc_results Output from gemtc::mtc.run, as returned in the 'mtcResults' list element from baye(). If this parameter is NULL then the frequentist analysis results found in 'contributions' are used exclusively. If it is not NULL then the Bayesian analysis results contained in this parameter are used as well. Defaults to NULL.
#' @return List of results with the described structure.
.PrepareComparisonsForCinema <- function(contributions, model_type, outcome_measure, gemtc_results = NULL) {
  if (model_type == "random") {
    comparisons <- names(contributions$x$prop.direct.random)
  } else if (model_type == "fixed") {
    comparisons <- names(contributions$x$prop.direct.common)
  } else {
    stop(glue::glue("Model type '{model_type}' is not supported. Please use 'random' or 'fixed'"))
  }
  
  
  prepared_analysis <- lapply(
    comparisons,
    function(comparison) {
      return(.PrepareComparisonForCinema(contributions$x, model_type, comparison, gemtc_results))
    }
  )
  
  return(prepared_analysis)
}


#' Creates a list containing the results of a comparison. The elements are analysis  results including treatment effects, standard errors, confidence intervals, predictions intervals, and p-values, for NMA effects, direct and indirect effects, and SIDE inconsistency factors. If there is no such data (e.g. if there is no indirect evidence) then the list element does not appear.
#'
#' @param freq_results Results from a frequentist model as contained in contribution output from {netmeta}.
#' @param model_type Type of model: "fixed" or "random".
#' @param comparison A comparison of the form <treatment1>:<treatment2>.
#' @param gemtc_results Output from gemtc::mtc.run, as returned in the 'mtcResults' list element from baye(). If this parameter is NULL then the frequentist analysis results found in 'contributions' are used exclusively. If it is not NULL then the Bayesian analysis results contained in this parameter are used as well. Defaults to NULL.
#' @return List of analysis results with the described structure.
.PrepareComparisonForCinema <- function(freq_results, model_type, comparison, gemtc_results = NULL) {
  if (model_type != "random" && model_type != "fixed") {
    stop(glue::glue("Model type '{model_type}' is not supported. Please use 'random' or 'fixed'"))
  }
  treatments <- stringr::str_extract(comparison, "^(.*?):(.*)$", c(1, 2))
  row <- treatments[1]
  col <- treatments[2]
  
  if (!is.null(gemtc_results)) {
    gemtc_stats <- ExtractGemtcStats(gemtc_results = gemtc_results, treatments = treatments)
    nma_treatment_effect <- gemtc_stats["median"]
    se_treat_effect <- gemtc_stats["se"]
    lower_ci <- gemtc_stats["ci_lower"]
    upper_ci <- gemtc_stats["ci_upper"]
    lower_pi <- gemtc_stats["pi_lower"]
    upper_pi <- gemtc_stats["pi_upper"]
  } else {
    lower_pi <- freq_results$lower.predict[row, col]
    upper_pi <- freq_results$upper.predict[row, col]
    if (model_type == "random") {
      nma_treatment_effect <- freq_results$TE.random[row, col]
      se_treat_effect <- freq_results$seTE.random[row, col]
      lower_ci <- freq_results$lower.random[row, col]
      upper_ci <- freq_results$upper.random[row, col]
    } else if (model_type == "fixed") {
      nma_treatment_effect <- freq_results$TE.common[row, col]
      se_treat_effect <- freq_results$seTE.common[row, col]
      lower_ci <- freq_results$lower.common[row, col]
      upper_ci <- freq_results$upper.common[row, col]
    }
  }
  
  if (model_type == "random") {
    direct <- freq_results$TE.direct.random[row, col]
    indirect <- freq_results$TE.indirect.random[row, col]
    
    direct_lower <- freq_results$lower.direct.random[row, col]
    direct_upper <- freq_results$upper.direct.random[row, col]
    indirect_lower <- freq_results$lower.indirect.random[row, col]
    indirect_upper <- freq_results$upper.indirect.random[row, col]
    
    sideif <- .CalculateSideif(direct, indirect, direct_lower, direct_upper, indirect_lower, indirect_upper)

    prepared_comparison <- list(
      "Direct" = jsonlite::unbox(direct),
      "DirectL" = jsonlite::unbox(direct_lower),
      "DirectU" = jsonlite::unbox(direct_upper),
      "Indirect" = jsonlite::unbox(indirect),
      "IndirectL" = jsonlite::unbox(indirect_lower),
      "IndirectU" = jsonlite::unbox(indirect_upper),
      "SideIF" = jsonlite::unbox(sideif$sideif),
      "SideIFlower" = jsonlite::unbox(sideif$sideif_lower),
      "SideIFupper" = jsonlite::unbox(sideif$sideif_upper),
      "SideZ" = jsonlite::unbox(sideif$sideif_z),
      "SidePvalue" = jsonlite::unbox(sideif$sideif_pval),
      "PropDir" = jsonlite::unbox(freq_results$prop.direct.random[comparison]),
      "NMA treatment effect" = jsonlite::unbox(nma_treatment_effect),
      "se treat effect" = jsonlite::unbox(se_treat_effect),
      "lower CI" = jsonlite::unbox(lower_ci),
      "upper CI" = jsonlite::unbox(upper_ci),
      "lower PrI" = jsonlite::unbox(lower_pi),
      "upper PrI" = jsonlite::unbox(upper_pi),
      "PropDirNetmeta" = jsonlite::unbox(freq_results$prop.direct.random[comparison]),
      "_row" = jsonlite::unbox(comparison)
    )
  } else if (model_type == "fixed") {
    direct <- freq_results$TE.direct.common[row, col]
    indirect <- freq_results$TE.indirect.common[row, col]
    
    direct_lower <- freq_results$lower.direct.common[row, col]
    direct_upper <- freq_results$upper.direct.common[row, col]
    indirect_lower <- freq_results$lower.indirect.common[row, col]
    indirect_upper <- freq_results$upper.indirect.common[row, col]
    
    sideif <- .CalculateSideif(direct, indirect, direct_lower, direct_upper, indirect_lower, indirect_upper)
    
    prepared_comparison <- list(
      "Direct" = jsonlite::unbox(direct),
      "DirectL" = jsonlite::unbox(direct_lower),
      "DirectU" = jsonlite::unbox(direct_upper),
      "Indirect" = jsonlite::unbox(indirect),
      "IndirectL" = jsonlite::unbox(indirect_lower),
      "IndirectU" = jsonlite::unbox(indirect_upper),
      "SideIF" = jsonlite::unbox(sideif$sideif),
      "SideIFlower" = jsonlite::unbox(sideif$sideif_lower),
      "SideIFupper" = jsonlite::unbox(sideif$sideif_upper),
      "SideZ" = jsonlite::unbox(sideif$sideif_z),
      "SidePvalue" = jsonlite::unbox(sideif$sideif_pval),
      "PropDir" = jsonlite::unbox(freq_results$prop.direct.common[comparison]),
      "NMA treatment effect" = jsonlite::unbox(nma_treatment_effect),
      "se treat effect" = jsonlite::unbox(se_treat_effect),
      "lower CI" = jsonlite::unbox(lower_ci),
      "upper CI" = jsonlite::unbox(upper_ci),
      "lower PrI" = jsonlite::unbox(lower_pi),
      "upper PrI" = jsonlite::unbox(upper_pi),
      "PropDirNetmeta" = jsonlite::unbox(freq_results$prop.direct.common[comparison]),
      "_row" = jsonlite::unbox(comparison)
    )
  }
  
  return(prepared_comparison[!is.na(prepared_comparison)])
}


#' Creates a named list containing SIDE inconsistency factor results for a comparison. The results are point estimate, confidence interval limits, Z-value and p-value.
#'
#' @param direct Direct treatment effect estimate.
#' @param indirect Indirect treatment effect estimate.
#' @param direct_lower Lower 95% CI limit of direct treatment effect.
#' @param direct_upper Upper 95% CI limit of direct treatment effect.
#' @param indirect_lower Lower 95% CI limit of indirect treatment effect.
#' @param indirect_upper Upper 95% CI limit of indirect treatment effect.
#' @return List of SIDE IF results with the described structure.
.CalculateSideif <- function(direct, indirect, direct_lower, direct_upper, indirect_lower, indirect_upper) {
  direct_se <- (direct_upper - direct_lower) / (2 * 1.96)
  indirect_se <- (indirect_upper - indirect_lower) / (2 * 1.96)
  
  sideif <- direct - indirect
  sideif_se <- sqrt((direct_se * direct_se) + (indirect_se * indirect_se))
  sideif_lower <- sideif - sideif_se * 1.96
  sideif_upper <- sideif + sideif_se * 1.96
  
  sideif_z <- sideif / sideif_se
  sideif_pval <- 2 * pnorm(q = -abs(sideif), mean = 0, sd = sideif_se)
  
  return(
    list(
      sideif = sideif,
      sideif_lower = sideif_lower,
      sideif_upper = sideif_upper,
      sideif_z = sideif_z,
      sideif_pval = sideif_pval
    )
  )
}


#' Extract the analysis statistics required for CINeMA from GEMTC output. This includes simulating a predictive distribution in order to get a prediction interval, which is not done in GEMTC.
#' 
#' @param gemtc_results
#' @param treatments Vector of two treatments
#' @return Named vector with names 'median', 'se', 'ci_lower', 'ci_upper', 'pi_lower', and 'pi_upper'.
ExtractGemtcStats <- function(gemtc_results, treatments) {
  #Put the treatment names in gemtc format
  gemtc_treatments <- glue::glue("d.{treatments[1]}.{treatments[2]}")
  mcmc_rel_eff <- gemtc::relative.effect(
    gemtc_results,
    t1 = treatments[1],
    t2 = treatments[2]
  )
  mcmc_summary <- summary(mcmc_rel_eff)
  quantiles <- mcmc_summary$summaries$quantiles
  
  if (is.element("sd.d", colnames(gemtc_results[[1]][[1]]))) {
    effects <- "random"
  } else {
    effects <- "fixed"
  }
  
  if (effects == "fixed") {
    median <- unname(quantiles["50%"])
    se <- unname(mcmc_summary$summaries$statistics["SD"])
    ci_lower <- unname(quantiles["2.5%"])
    ci_upper <- unname(quantiles["97.5%"])
  } else if (effects == "random") {
    median <- quantiles[gemtc_treatments, "50%"]
    se <- mcmc_summary$summaries$statistics[gemtc_treatments, "SD"]
    ci_lower <- quantiles[gemtc_treatments, "2.5%"]
    ci_upper <- quantiles[gemtc_treatments, "97.5%"]
  }
  
  if (effects == "fixed") {
    pi_lower <- ci_lower
    pi_upper <- ci_upper
  } else if (effects == "random") {
    #Simulate a predictive distribution manually, using the treatment effect and heterogeneity standard deviation at each iteration. Do this once for each chain (of which there are always 4).
    n_iterations <- length(mcmc_rel_eff$samples[[1]][, 1])
    prediction_samples <- lapply(
      X = 1:4,
      FUN = function(chain) {
        matrix(
          rnorm(
            n = n_iterations,
            mean = mcmc_rel_eff$samples[[chain]][, gemtc_treatments],
            sd = mcmc_rel_eff$samples[[chain]][, "sd.d"]
          ),
          dimnames = list(NULL, "prediction")
        )
      }
    )
  
    pi_lower <- MCMCvis::MCMCsummary(prediction_samples)[, "2.5%"]
    pi_upper <- MCMCvis::MCMCsummary(prediction_samples)[, "97.5%"]
  }

  return(c(median = median, se = se, ci_lower =  ci_lower, ci_upper = ci_upper, pi_lower = pi_lower, pi_upper = pi_upper))
}
