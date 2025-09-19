test_that("Check baseline_ranking function works as expected", {
  result <- baseline_ranking()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_ranking")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_ranking")
  app$click("baseline_ranking-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

