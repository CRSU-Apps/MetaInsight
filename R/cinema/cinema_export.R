
DELETEME_EXAMPLE_CONTRIBUTION_MATRIX = list(
  "A:B" = list(
    "1" = jsonlite::unbox(1),
    "2" = jsonlite::unbox(2),
    "3" = jsonlite::unbox(3),
    "4" = jsonlite::unbox(4),
    "5" = jsonlite::unbox(5),
    "6" = jsonlite::unbox(6),
    "7" = jsonlite::unbox(7),
    "8" = jsonlite::unbox(8)
  ),
  "A:C" = list(
    "1" = jsonlite::unbox(11),
    "2" = jsonlite::unbox(12),
    "3" = jsonlite::unbox(13),
    "4" = jsonlite::unbox(14),
    "5" = jsonlite::unbox(15),
    "6" = jsonlite::unbox(16),
    "7" = jsonlite::unbox(17),
    "8" = jsonlite::unbox(18)
  ),
  "A:D" = list(
    "1" = jsonlite::unbox(21),
    "2" = jsonlite::unbox(22),
    "3" = jsonlite::unbox(23),
    "4" = jsonlite::unbox(24),
    "5" = jsonlite::unbox(25),
    "6" = jsonlite::unbox(26),
    "7" = jsonlite::unbox(27),
    "8" = jsonlite::unbox(28)
  ),
  "B:C" = list(
    "1" = jsonlite::unbox(31),
    "2" = jsonlite::unbox(32),
    "3" = jsonlite::unbox(33),
    "4" = jsonlite::unbox(34),
    "5" = jsonlite::unbox(35),
    "6" = jsonlite::unbox(36),
    "7" = jsonlite::unbox(37),
    "8" = jsonlite::unbox(38)
  ),
  "B:D" = list(
    "1" = jsonlite::unbox(41),
    "2" = jsonlite::unbox(42),
    "3" = jsonlite::unbox(43),
    "4" = jsonlite::unbox(44),
    "5" = jsonlite::unbox(45),
    "6" = jsonlite::unbox(46),
    "7" = jsonlite::unbox(47),
    "8" = jsonlite::unbox(48)
  ),
  "C:D" = list(
    "1" = jsonlite::unbox(51),
    "2" = jsonlite::unbox(52),
    "3" = jsonlite::unbox(53),
    "4" = jsonlite::unbox(54),
    "5" = jsonlite::unbox(55),
    "6" = jsonlite::unbox(56),
    "7" = jsonlite::unbox(57),
    "8" = jsonlite::unbox(58)
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

CinemaProjectTest <- function() {
  data <- CleanData(read.csv("tests/testthat/data/cinema_data/NMA_data_binary_FE_two_arm_CINeMA.csv"))
  outcome_type = "Binary"
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments)
  data <- WrangleUploadData(data, treatment_ids, outcome_type)
  
  data <-ReplaceTreatmentIds(data, treatment_ids)
  
  model_type = "fixed"
  outcome_measure = "OR"
  
  pairwise1 <- meta::pairwise(
    treat = T,
    event = R,
    n = N,
    studlab = Study,
    data = data,
    sm = outcome_measure
  )
  
  netmeta1 <- netmeta::netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = Study,
    common = model_type == "fixed",
    random = model_type == "random",
    data = pairwise1,
    sm = outcome_measure
  )
  
  contributions <- netmeta::netcontrib(
    x = netmeta1,
    method = "shortestpath"
  )
  
  prepped_project <- PrepareProjectForCinema(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure)
  
  jsonlite::toJSON(prepped_project, pretty = TRUE)
}