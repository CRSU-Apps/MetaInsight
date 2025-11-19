test_that("Check baseline_results function works as expected", {
  result <- baseline_results(fitted_baseline_model)
  expect_is(result, "html")
  expect_equal(stringr::str_count(result, "<br"), 5)
})

test_that("Check baseline_results function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(baseline_results("faulty_model"), "model must be an object created by baseline_model")
  expect_error(baseline_results(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(baseline_results(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_baseline_results", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_results")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_results")
  app$click("baseline_results-run")
  app$wait_for_idle()
  app$wait_for_value(output = "baseline_results-all-statistics")

  stats_all <- app$get_value(output = "baseline_results-all-statistics")
  expect_match(stats_all, "<table")
  # Test number of rows (including header)
  expect_equal(stringr::str_count(stats_all, "<tr"), 14)
  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(stats_all, "<tr>.+?</tr>"), "<th"), 5)

  quant_all <- app$get_value(output = "baseline_results-all-quantiles")
  expect_match(quant_all, "<table")
  # Test number of rows (including header)
  expect_equal(stringr::str_count(quant_all, "<tr"), 14)
  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(quant_all, "<tr>.+?</tr>"), "<th"), 6)

  text_all <- app$get_value(output = "baseline_results-all-text")
  expect_match(text_all$html, "Iterations")

  app$stop()

})

