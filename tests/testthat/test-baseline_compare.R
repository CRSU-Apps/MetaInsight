test_that("Check baseline_compare function works as expected", {
  result <- baseline_compare(fitted_baseline_model)
  expect_is(result, "matrix")
  expect_equal(nrow(result), n_trt_all)
  expect_equal(ncol(result), n_trt_all)
})

test_that("Check bayes_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(baseline_compare("faulty_model"), "model must be an object created by baseline_model")
  expect_error(baseline_compare(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(baseline_compare(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_baseline_compare", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_summary", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_compare")
  app$click("baseline_compare-run")

  app$wait_for_value(output = "baseline_compare-baseline-table")
  table <- app$get_value(output = "baseline_compare-baseline-table")

  expect_match(table, "<table")

  # Test number of rows (including header)
  expect_equal(stringr::str_count(table, "<tr"), n_trt_all + 1)

  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(table, "<tr>.+?</tr>"), "<th"), n_trt_all + 1)

  table_all_dl <- app$get_download("baseline_compare-baseline-download")
  df <- read.csv(table_all_dl)
  # extra column which is hidden in app, header becomes names
  expect_equal(nrow(df), n_trt_all)
  expect_equal(ncol(df), n_trt_all + 1)

  app$stop()
})

