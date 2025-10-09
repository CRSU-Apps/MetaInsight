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
  
  long_data <- ReinstateTreatmentIds(long_data, treatment_ids)
  
  prepared_data <- lapply(
    1:nrow(long_data),
    function(index) {
      item = list()
      
      item$study <- jsonlite::unbox(long_data[index, "Study"])
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
  
  prepared_hat_matrix <- list(
    colNames = colnames(hat_matrix),
    H = hat_matrix,
    model = jsonlite::unbox(model_type),
    rowNames = rownames(hat_matrix),
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
  return(jsonlite::toJSON(prepared_project, pretty = TRUE))
}