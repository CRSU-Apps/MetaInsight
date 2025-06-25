
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
  
  model_type = "fixed"
  outcome_measure = "OR"
  
  non_covariate_data <- RemoveCovariates(data)
  reference <- treatment_ids$Label[treatment_ids$Number == 1]
  
  if (FindDataShape(non_covariate_data) == "long") {
    non_covariate_data_wide <- LongToWide(long_data = non_covariate_data, outcome_type = outcome_type)
  } else {
    non_covariate_data_wide <- non_covariate_data
  }
  
  # transform data to contrast form
  d0 <- contrastform.df(non_covariate_data_wide, treatment_ids, outcome_measure, outcome_type)
  #obtain treatment labels
  lstx <- treatment_ids$Label
  #count treatment numbers
  ntx <- length(lstx)
  #matching treatment labels to treatment code
  d1 <- labelmatching.df(d1 = d0, ntx = ntx, treat_list = treatment_ids)
  # NMA of all studies
  net1 <- freq.df(model = model_type, outcome = outcome_measure, dataf = d1, ref = reference)
  
  freq_all <- list(
    net1 = net1,
    lstx = lstx,
    ntx = ntx,
    d0 = d0,
    d1 = d1
  )
  
  contributions <- netmeta::netcontrib(
    x = freq_all$net1,
    method = "shortestpath"
  )
  
  prepped_project <- PrepareProjectForCinema(data, treatment_ids, outcome_type, contributions, model_type, outcome_measure)
  
  jsonlite::toJSON(prepped_project, pretty = TRUE)
}