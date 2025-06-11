
DELETEME_EXAMPLE_CONTRIBUTION_MATRIX = list(
  "one:two" = list(
    "1" = jsonlite::unbox(1),
    "2" = jsonlite::unbox(2),
    "3" = jsonlite::unbox(3)
  ),
  "one:three" = list(
    "1" = jsonlite::unbox(11),
    "2" = jsonlite::unbox(12),
    "3" = jsonlite::unbox(13)
  ),
  "two:three" = list(
    "1" = jsonlite::unbox(21),
    "2" = jsonlite::unbox(22),
    "3" = jsonlite::unbox(23)
  )
)

PrepareDataForCinema <- function(data, treatment_ids, outcome_type) {
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome_type)
  } else {
    long_data <- data
  }
  
  prepared_data <- lapply(
    1:nrow(long_data),
    function(index) {
      item = list()
      
      item$study <- jsonlite::unbox(long_data[index, "Study"])
      item$id <- jsonlite::unbox(long_data[index, "StudyID"])
      item$t <- jsonlite::unbox(treatment_ids$Label[treatment_ids$Number == long_data[index, "T"]])
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
  } else if (model_type == "fixed") {
    hat_matrix <- analysis$random
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
        studycontributions = DELETEME_EXAMPLE_CONTRIBUTION_MATRIX
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

################################################################################

CinemaDataTest <- function() {
  cinema_data <- LoadCinemaData()
  prepped_data <- PrepareDataForCinema(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type)
  
  jsonlite::toJSON(prepped_data, pretty = TRUE)
}

CinemaHatMatrixTest <- function() {
  cinema_data <- LoadCinemaData()
  
  model_type = "fixed"
  outcome_measure = "OR"
  cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)
  
  prepped_analysis <- PrepareAnalysisForCinema(cinema_analysis, model_type, outcome_measure)
  
  jsonlite::toJSON(prepped_analysis, pretty = TRUE)
}

CinemaProjectTest <- function() {
  cinema_data <- LoadCinemaData()
  
  model_type = "fixed"
  outcome_measure = "OR"
  cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)
  
  prepped_project <- PrepareProjectForCinema(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type, cinema_analysis, model_type, outcome_measure)
  
  jsonlite::toJSON(prepped_project, pretty = TRUE)
}

CinemaProjectSchemaTest <- function() {
  cinema_data <- LoadCinemaData()
  
  model_type = "fixed"
  outcome_measure = "OR"
  cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)
  
  prepped_project <- PrepareProjectForCinema(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type, cinema_analysis, model_type, outcome_measure)
  
  json <- jsonlite::toJSON(prepped_project, pretty = TRUE)
  
  result <- jsonvalidate::json_validate(json, "cinema/cinema_schema.json", verbose = TRUE)
  
  if (!result) {
    print(json)
  }
  print(result)
}