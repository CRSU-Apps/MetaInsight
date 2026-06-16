GenerateCinemaAnalysis <- function(cinema_data) {
  contributions <- netmeta::netcontrib(
    x = cinema_data$freq$netmeta,
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
  cinema_long_data <- ReinstateTreatmentIds(cinema_long_data, treatment_ids)

  data <- list(
    is_data_valid = TRUE,
    data = cinema_long_data,
    treatments = treatment_ids,
    outcome = cinema_outcome
  )
  class(data) <- "loaded_data"

  config <- setup_configure(data, "Placebo", cinema_effects, cinema_outcome_measure, "good", 123)

  calculated_analysis <- GenerateCinemaAnalysis(config)

  exported_json <- export_cinema(config)
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

valid_schema <- file.path(test_data_dir, "cinema_data", "cinema_schema.json")

test_that("Should produce valid JSON", {
  json <- export_cinema(cinema_con)
  result <- jsonvalidate::json_validate(json, valid_schema, verbose = TRUE)

  expect_true(result, label = result)
})

test_that("export_cinema produces errors when given incorrect data or whwn rob data does not exist", {
  expect_error(export_cinema("not data"), "configured_data must be of class configured_data")
  expect_error(export_cinema(configured_data_bin), "uploaded data must contain")
  expect_error(export_cinema(configured_data_con, "not gemtc"), "gemtc_results must be of class mtc.result")
  expect_error(export_cinema(configured_data_con, fitted_baseline_model$mtcResults), "gemtc_results must be of class mtc.result")
  expect_error(export_cinema(configured_data_con, fitted_covariate_model$mtcResults), "regression models cannot currently be exported")
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
  testthat::skip("because CINeMA uses an old version of {netmeta}")
  expect_equal_and_not_na(
    jsonlite::flatten(imported_project$project$CM$contributionMatrices$studycontributions),
    jsonlite::flatten(exported_project$project$CM$contributionMatrices$studycontributions),
    "study contributions",
    equality_expectation = function(actual, expected) {
      expect_data_frames_equal(actual, expected, numerical_tolerance = 0.1)
    }
  )
})


test_that("export_cinema exports frequentist results", {
  skip_if(skip_shinytest2)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_export_cinema")
  reload_app(app, config_path)
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "export_cinema")

  json_all <- app$get_download("export_cinema-download")
  result_all <- jsonvalidate::json_validate(json_all, valid_schema, verbose = TRUE)
  expect_true(result_all)

  app$set_inputs("export_cinema-data" = "subsetted_data")
  json_sub <- app$get_download("export_cinema-download")
  result_sub <- jsonvalidate::json_validate(json_sub, valid_schema, verbose = TRUE)
  expect_true(result_sub)
  expect_false(identical(jsonlite::read_json(json_all), jsonlite::read_json(json_sub)))

  sub_lines <- jsonlite::read_json(json_sub)
  sub_data <- dplyr::bind_rows(sub_lines$project$studies$long)
  expect_false("Minerva" %in% sub_data$study)
})

test_that("export_cinema exports Bayesian results", {
  skip_if(skip_shinytest2)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_export_cinema")
  reload_app(app, bayes_model_path)
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "export_cinema")
  app$set_inputs("export_cinema-model" = "bayes")

  json_all <- app$get_download("export_cinema-download")
  result_all <- jsonvalidate::json_validate(json_all, valid_schema, verbose = TRUE)
  expect_true(result_all)

  app$set_inputs("export_cinema-data" = "subsetted_data")
  json_sub <- app$get_download("export_cinema-download")
  result_sub <- jsonvalidate::json_validate(json_sub, valid_schema, verbose = TRUE)
  expect_true(result_sub)
  expect_false(identical(jsonlite::read_json(json_all), jsonlite::read_json(json_sub)))

  sub_lines <- jsonlite::read_json(json_sub)
  sub_data <- dplyr::bind_rows(sub_lines$project$studies$long)
  expect_false("Minerva" %in% sub_data$study)
})
