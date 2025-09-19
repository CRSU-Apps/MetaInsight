test_that("Check baseline_regression function works as expected", {
  result <- baseline_regression()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_regression", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_regression")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_regression")
  app$click("baseline_regression-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

