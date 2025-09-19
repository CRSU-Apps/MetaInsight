test_that("Check baseline_forest function works as expected", {
  result <- baseline_forest()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_forest", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_forest")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_forest")
  app$click("baseline_forest-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

