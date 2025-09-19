test_that("Check baseline_comparison function works as expected", {
  result <- baseline_comparison()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_comparison", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_comparison")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_comparison")
  app$click("baseline_comparison-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

