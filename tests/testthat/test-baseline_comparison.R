test_that("Check baseline_comparison function works as expected", {
  result <- baseline_comparison(fitted_baseline_model)
  expect_is(result, "matrix")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
})

test_that("Check bayes_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(baseline_comparison("faulty_model"), "model must be an object created by baseline_model")
  expect_error(baseline_comparison(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(baseline_comparison(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_baseline_comparison", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_summary", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_comparison")
  app$click("baseline_comparison-run")

  app$wait_for_value(output = "baseline_comparison-baseline-table")
  table <- app$get_value(output = "baseline_comparison-baseline-table")

  expect_match(table, "<table")

  # Test number of rows (including header)
  expect_equal(stringr::str_count(table, "<tr"), 7)

  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(table, "<tr>.+?</tr>"), "<th"), 7)

  table_all_dl <- app$get_download("baseline_comparison-baseline-download")
  df <- read.csv(table_all_dl)
  # extra column which is hidden in app, header becomes names
  expect_equal(nrow(df), 6)
  expect_equal(ncol(df), 7)

  app$stop()
})

