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
.PrepareStudyContibutionsForCinema <- function(study_contributions) {
  prepared_contributions <- sapply(
    simplify = FALSE,
    unique(study_contributions$comparison),
    function(comparison) {
      return(
        sapply(
          simplify = FALSE,
          study_contributions$study[study_contributions$comparison == comparison],
          function(study) {
            return(
              jsonlite::unbox(
                study_contributions$contribution[study_contributions$comparison == comparison & study_contributions$study == study]
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
#' @return A List with the described structure.
.PrepareAnalysisForCinema <- function(contributions, model_type, outcome_measure) {
  if (model_type == "fixed") {
    hat_matrix <- contributions$common
    study_contributions <- contributions$study.common
  } else if (model_type == "random") {
    hat_matrix <- contributions$random
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
    colNames = colnames(hat_matrix),
    colNamesNMAresults = nma_col_names,
    H = hat_matrix,
    model = jsonlite::unbox(model_type),
    NMAresults = .PrepareComparisonsForCinema(contributions, model_type, outcome_measure),
    rowNames = rownames(hat_matrix),
    rowNamesNMAresults = rownames(hat_matrix),
    sm = jsonlite::unbox(outcome_measure)
  )
  
  prepared_analysis <- list(
    contributionMatrices = c(
      list(
        hatmatrix = prepared_hat_matrix,
        studycontributions = .PrepareStudyContibutionsForCinema(study_contributions)
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
#' @return A List with the described structure.
.PrepareProjectForCinema <- function(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure) {
  prepared_data <- .PrepareDataForCinema(data, treatment_ids, outcome_type)
  prepared_analysis <- .PrepareAnalysisForCinema(contributions, model_type, outcome_measure)
  
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
#' @return JSON string with the described structure.
GenerateCinemaJson <- function(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure) {
  prepared_project <- .PrepareProjectForCinema(
    data,
    treatment_ids,
    outcome_type,
    contributions,
    model_type,
    outcome_measure
  )
  return(jsonlite::toJSON(prepared_project, pretty = TRUE, na = "null"))
}



































.PrepareComparisonsForCinema <- function(contributions, model_type, outcome_measure) {
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
      return(.PrepareComparisonForCinema(contributions$x, model_type, comparison))
    }
  )
  
  return(prepared_analysis)
}

.PrepareComparisonForCinema <- function(contributions, model_type, comparison) {
  if (model_type != "random" && model_type != "fixed") {
    stop(glue::glue("Model type '{model_type}' is not supported. Please use 'random' or 'fixed'"))
  }
  
  x <<- contributions
  
  treatments <- stringr::str_extract(comparison, "^(.*?):(.*)$", c(1, 2))
  row <- treatments[1]
  col <- treatments[2]
  
  if (model_type == "random") {
    direct <- contributions$TE.direct.random[row, col]
    indirect <- contributions$TE.indirect.random[row, col]
    
    direct_lower <- contributions$lower.direct.random[row, col]
    direct_upper <- contributions$upper.direct.random[row, col]
    indirect_lower <- contributions$lower.indirect.random[row, col]
    indirect_upper <- contributions$upper.indirect.random[row, col]
    
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
      "PropDir" = jsonlite::unbox(contributions$prop.direct.random[comparison]),
      "NMA treatment effect" = jsonlite::unbox(contributions$TE.random[row, col]),
      "se treat effect" = jsonlite::unbox(contributions$seTE.random[row, col]),
      "lower CI" = jsonlite::unbox(contributions$lower.random[row, col]),
      "upper CI" = jsonlite::unbox(contributions$upper.random[row, col]),
      "lower PrI" = jsonlite::unbox(contributions$lower.predict[row, col]),
      "upper PrI" = jsonlite::unbox(contributions$upper.predict[row, col]),
      "PropDirNetmeta" = jsonlite::unbox(contributions$prop.direct.random[comparison]),
      "_row" = jsonlite::unbox(comparison)
    )
  } else if (model_type == "fixed") {
    direct <- contributions$TE.direct.common[row, col]
    indirect <- contributions$TE.indirect.common[row, col]
    
    direct_lower <- contributions$lower.direct.common[row, col]
    direct_upper <- contributions$upper.direct.common[row, col]
    indirect_lower <- contributions$lower.indirect.common[row, col]
    indirect_upper <- contributions$upper.indirect.common[row, col]
    
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
      "PropDir" = jsonlite::unbox(contributions$prop.direct.common[comparison]),
      "NMA treatment effect" = jsonlite::unbox(contributions$TE.common[row, col]),
      "se treat effect" = jsonlite::unbox(contributions$seTE.common[row, col]),
      "lower CI" = jsonlite::unbox(contributions$lower.common[row, col]),
      "upper CI" = jsonlite::unbox(contributions$upper.common[row, col]),
      "lower PrI" = jsonlite::unbox(contributions$lower.predict[row, col]),
      "upper PrI" = jsonlite::unbox(contributions$upper.predict[row, col]),
      "PropDirNetmeta" = jsonlite::unbox(contributions$prop.direct.common[comparison]),
      "_row" = jsonlite::unbox(comparison)
    )
  }
  
  return(prepared_comparison[!is.na(prepared_comparison)])
}

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
