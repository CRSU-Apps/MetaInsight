test_that("Check baseline_results function works as expected", {
  result <- baseline_results()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_results", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_results")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_results")
  app$click("baseline_results-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

