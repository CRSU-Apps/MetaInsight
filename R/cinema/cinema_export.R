
prepareStudyContibutionsForCinema <- function(study_contributions) {
  prepped_contributions <- sapply(
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
  
  return(prepped_contributions)
}

PrepareDataForCinema <- function(data, treatment_ids, outcome_type) {
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

PrepareAnalysisForCinema <- function(analysis, model_type, outcome_measure) {
  if (model_type == "fixed") {
    hat_matrix <- analysis$common
    study_contributions <- analysis$study.common
  } else if (model_type == "fixed") {
    hat_matrix <- analysis$random
    study_contributions <- analysis$random
  } else {
    stop(glue::glue("Model type'{model_type}' not supported"))
  }
  
  prepped_hat_matrix <- list(
    colNames = colnames(hat_matrix),
    H = hat_matrix,
    model = jsonlite::unbox(model_type),
    rowNames = rownames(hat_matrix),
    sm = jsonlite::unbox(outcome_measure)
  )
  
  prepared_analysis <- list(
    contributionMatrices = c(
      list(
        hatmatrix = prepped_hat_matrix,
        studycontributions = prepareStudyContibutionsForCinema(study_contributions)
      )
    )
  )
  
  return(prepared_analysis)
}

PrepareProjectForCinema <- function(long_data, treatment_ids, outcome_type, analysis, model_type, outcome_measure) {
  prepped_data <- PrepareDataForCinema(long_data, treatment_ids, outcome_type)
  prepared_analysis <- PrepareAnalysisForCinema(analysis, model_type, outcome_measure)
  
  prepped_project = list(
    project = list(
      CM = prepared_analysis,
      format = jsonlite::unbox("long"),
      type = jsonlite::unbox(tolower(outcome_type)),
      studies = list(
        long = prepped_data
      )
    )
  )
  
  return(prepped_project)
}