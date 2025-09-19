test_that("Check baseline_details function works as expected", {
  result <- baseline_details()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_details")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_details")
  app$click("baseline_details-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

