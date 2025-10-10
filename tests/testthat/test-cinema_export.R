
LoadCinemaData <- function(file_path = "data/cinema_data/NMA_data_binary_FE_two_arm_CINeMA.csv") {
  data <- CleanData(read.csv(file_path))
  outcome_type = "Binary"
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments)
  data <- WrangleUploadData(data, treatment_ids, outcome_type)
  
  return(
    list(
      long_data = data,
      treatment_ids = treatment_ids,
      outcome_type = outcome_type
    )
  )
}

GenerateCinemaAnalysis <- function(cinema_data, model_type, outcome_measure) {
  reference = cinema_data$treatment_ids$Label[cinema_data$treatment_ids$Number == 1]
  wide_data <- LongToWide(long_data = cinema_data$long_data, outcome_type = cinema_data$outcome_type)
  
  # transform data to contrast form
  d0 <- contrastform.df(wide_data, cinema_data$treatment_ids, outcome_measure, cinema_data$outcome_type)
  #obtain treatment labels
  lstx <- cinema_data$treatment_ids$Label
  #count treatment numbers
  ntx <- length(lstx)
  #matching treatment labels to treatment code
  d1 <- labelmatching.df(d1 = d0, ntx = ntx, treat_list = cinema_data$treatment_ids)
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
    method = "shortestpath",
    study = TRUE
  )
  
  return(contributions)
}

RecreateAnalysisFromCinemaProject <- function() {
  cinema_project_json <- readr::read_file("data/cinema_data/diabetes_basic.cnm")
  cinema_project <- jsonlite::fromJSON(cinema_project_json)
  
  cinema_model_type <- unlist(cinema_project$project$CM$contributionMatrices$hatmatrix$model)
  cinema_outcome_type <- unlist(stringr::str_to_sentence(cinema_project$project$type))
  cinema_outcome_measure <- unlist(cinema_project$project$CM$contributionMatrices$hatmatrix$sm)
  
  cinema_long_data <- cinema_project$project$studies$long
  treatment_ids <- cinema_long_data |>
    FindAllTreatments() |>
    CreateTreatmentIds() |>
    CleanTreatmentIds()
  cinema_long_data <- WrangleUploadData(cinema_long_data, treatment_ids, cinema_outcome_type)
  
  data <- list(
    long_data = cinema_long_data,
    treatment_ids = treatment_ids,
    outcome_type = cinema_outcome_type
  )
  
  calculated_analysis <- GenerateCinemaAnalysis(data, cinema_model_type, cinema_outcome_measure)
  
  exported_json <- GenerateCinemaJson(
    cinema_long_data,
    treatment_ids,
    cinema_outcome_type,
    calculated_analysis,
    cinema_model_type,
    cinema_outcome_measure
  )
  exported_project <- jsonlite::fromJSON(exported_json)
  
  return(
    list(
      imported_project = cinema_project,
      exported_project = exported_project
    )
  )
}

expect_equal_and_not_na <- function(actual, expected, item_name, equality_expectation=expect_equal) {
  # Unbox single item lists
  if (typeof(actual) == "list" && length(actual) == 1) {
    actual = unlist(actual)
  }
  if (typeof(expected) == "list" && length(expected) == 1) {
    expected = unlist(expected)
  }
  
  expect_equal(typeof(actual), typeof(expected))
  
  expect_false(all(is.na(actual), label = glue::glue("Actual {item_name} should not be NA")))
  expect_false(all(is.null(expected), label = glue::glue("Expected {item_name} should not be NA")))
  
  expect_false(all(is.na(actual), label = glue::glue("Actual {item_name} should not be NULL")))
  expect_false(all(is.null(expected), label = glue::glue("Expected {item_name} should not be NULL")))
  
  equality_expectation(actual, expected)
}

expect_data_frames_equal <- function(actual, expected, numerical_tolerance=0.0002) {
  if (class(actual) != "data.frame" || class(expected) != "data.frame") {
    stop("Not data frames")
  }
  
  expect_equal(nrow(actual), nrow(expected))
  expect_equal(ncol(actual), ncol(expected))
  
  matches <- lapply(
    1:nrow(expected),
    function(index) {
      row_matches <- rep(TRUE, nrow(expected))
      
      for (name in names(expected)) {
        if (is.na(expected[[name]][index])) {
          row_matches <- row_matches & is.na(actual[[name]])
        } else if (is.numeric(expected[[name]][index])) {
          row_matches <- row_matches & (abs(actual[[name]] - expected[[name]][index]) <= numerical_tolerance)
        } else {
          row_matches <- row_matches & actual[[name]] == expected[[name]][index]
        }
      }
      
      return(length(which(row_matches)) == 1)
    }
  )
  
  expect_true(all(unlist(matches)))
}


recreated_project <- RecreateAnalysisFromCinemaProject()
imported_project <- recreated_project$imported_project
exported_project <- recreated_project$exported_project


test_that("Should produce valid JSON", {
  cinema_data <- LoadCinemaData()
  model_type <- "fixed"
  outcome_measure <- "OR"
  cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)

  json <- GenerateCinemaJson(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type, cinema_analysis, model_type, outcome_measure)
  result <- jsonvalidate::json_validate(json, "../../cinema/cinema_schema.json", verbose = TRUE)

  expect_true(result, label = result)
})

test_that("Should export analysis settings", {
  expect_equal_and_not_na(
    exported_project$project$type,
    imported_project$project$type,
    "outcome type"
  )
  expect_equal_and_not_na(
    exported_project$project$format,
    imported_project$project$format,
    "data format"
  )
  expect_equal_and_not_na(
    exported_project$project$CM$contributionMatrices$hatmatrix$model,
    imported_project$project$CM$contributionMatrices$hatmatrix$model,
    "model type"
  )
  expect_equal_and_not_na(
    exported_project$project$CM$contributionMatrices$hatmatrix$sm,
    imported_project$project$CM$contributionMatrices$hatmatrix$sm,
    "outcome measure"
  )
})

test_that("Should export analysis data", {
  expect_equal_and_not_na(
    exported_project$project$studies$long,
    imported_project$project$studies$long,
    "data",
    equality_expectation = expect_data_frames_equal
  )
})

test_that("Should export expected row and column names", {
  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$rowNames,
    exported_project$project$CM$contributionMatrices$hatmatrix$rowNames,
    "row names"
  )

  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$colNames,
    exported_project$project$CM$contributionMatrices$hatmatrix$colNames,
    "column names"
  )

  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$rowNamesNMAresults,
    exported_project$project$CM$contributionMatrices$hatmatrix$rowNamesNMAresults,
    "NMA result row names"
  )

  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$colNamesNMAresults,
    exported_project$project$CM$contributionMatrices$hatmatrix$colNamesNMAresults,
    "NMA result column names"
  )
})

test_that("Should export NMA results", {
  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$NMAresults[[1]],
    exported_project$project$CM$contributionMatrices$hatmatrix$NMAresults,
    "NMA results",
    equality_expectation = expect_data_frames_equal
  )
})

test_that("Should export H matrix", {
  # expect_equal_and_not_na(
  #   imported_project$project$CM$contributionMatrices$hatmatrix$H[[1]],
  #   exported_project$project$CM$contributionMatrices$hatmatrix$H,
  #   "H matrix"
  # )
})

test_that("Should export study contributions", {
  # expect_equal_and_not_na(
  #   imported_project$project$CM$contributionMatrices$studycontributions[[1]],
  #   exported_project$project$CM$contributionMatrices$studycontributions,
  #   "study contributions"
  # )
})
