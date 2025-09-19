test_that("Check baseline_summary function works as expected", {
  result <- baseline_summary()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_summary", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_summary")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_summary")
  app$click("baseline_summary-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

