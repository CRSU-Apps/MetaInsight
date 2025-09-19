test_that("Check baseline_model function works as expected", {
  result <- baseline_model()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_model")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_model")
  app$click("baseline_model-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

