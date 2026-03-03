GenerateCinemaAnalysis <- function(cinema_data) {
  contributions <- netmeta::netcontrib(
    x = cinema_data$freq$net1,
    method = "shortestpath",
    study = TRUE
  )

  return(contributions)
}

RecreateAnalysisFromCinemaProject <- function() {
  cinema_project_json <- readLines(file.path(test_data_dir, "cinema_data", "diabetes_basic.cnm"))
  cinema_project <- jsonlite::fromJSON(cinema_project_json)
  cinema_project$project$studies$long$study <- CleanStrings(cinema_project$project$studies$long$study)

  cinema_effects <- unlist(cinema_project$project$CM$contributionMatrices$hatmatrix$model)
  cinema_outcome <- tolower(unlist(stringr::str_to_sentence(cinema_project$project$type)))
  cinema_outcome_measure <- unlist(cinema_project$project$CM$contributionMatrices$hatmatrix$sm)

  cinema_long_data <- cinema_project$project$studies$long
  treatment_ids <- cinema_long_data |>
    FindAllTreatments() |>
    CreateTreatmentIds() |>
    CleanTreatmentIds()
  cinema_long_data <- WrangleUploadData(cinema_long_data, treatment_ids, cinema_outcome)
  cinema_long_data <- metainsight:::ReinstateTreatmentIds(cinema_long_data, treatment_ids)

  data <- list(
    is_data_valid = TRUE,
    data = cinema_long_data,
    treatments = treatment_ids,
    outcome = cinema_outcome
  )
  class(data) <- "loaded_data"

  config <- setup_configure(data, data$treatments$Label[1], cinema_effects, cinema_outcome_measure, "good", 123)

  calculated_analysis <- GenerateCinemaAnalysis(config)

  exported_json <- rep_cinema(config)
  exported_project <- jsonlite::fromJSON(exported_json)

  return(
    list(
      imported_project = cinema_project,
      exported_project = exported_project
    )
  )
}


recreated_project <- RecreateAnalysisFromCinemaProject()
imported_project <- recreated_project$imported_project
exported_project <- recreated_project$exported_project


test_that("Should produce valid JSON", {
  json <- rep_cinema(cinema_con)
  result <- jsonvalidate::json_validate(json, file.path(test_data_dir, "cinema_data", "cinema_schema.json"), verbose = TRUE)

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

  # "_row" column is a special case which is handled differently
  exportedColNames <- exported_project$project$CM$contributionMatrices$hatmatrix$colNamesNMAresults[[1]]
  exportedColNames <- exportedColNames[-which(exportedColNames == "_row")]

  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$colNamesNMAresults[[1]],
    exportedColNames,
    "NMA result column names"
  )
})

test_that("Should export NMA results", {
  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$NMAresults[[1]],
    exported_project$project$CM$contributionMatrices$hatmatrix$NMAresults[[1]],
    "NMA results",
    equality_expectation = expect_data_frames_equal
  )

  expect_equal(imported_project$project$CM$contributionMatrices$hatmatrix$NMAresults[[1]],
               exported_project$project$CM$contributionMatrices$hatmatrix$NMAresults[[1]], tolerance = 0.0002)
})

test_that("Should export H matrix", {
  expect_equal_and_not_na(
    imported_project$project$CM$contributionMatrices$hatmatrix$H[[1]],
    exported_project$project$CM$contributionMatrices$hatmatrix$H[[1]],
    "H matrix",
    equality_expectation = function(actual, expected) {
      expect_matrices_equal(actual, expected, numerical_tolerance = 0.1)
    }
  )

  expect_equal(imported_project$project$CM$contributionMatrices$hatmatrix$H[[1]],
               exported_project$project$CM$contributionMatrices$hatmatrix$H[[1]],
               tolerance = 0.1)

})

test_that("Should export study contributions", {
  # Test skipped because CINeMA uses an old version of {netmeta}
  # expect_equal_and_not_na(
  #   jsonlite::flatten(imported_project$project$CM$contributionMatrices$studycontributions),
  #   jsonlite::flatten(exported_project$project$CM$contributionMatrices$studycontributions),
  #   "study contributions",
  #   equality_expectation = function(actual, expected) {
  #     expect_data_frames_equal(actual, expected, numerical_tolerance = 0.1)
  #   }
  # )
})








test_that("{shinytest2} recording: e2e_rep_cinema", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_rep_cinema")
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_cinema")
  app$click("rep_cinema-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

