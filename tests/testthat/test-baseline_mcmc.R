test_that("Check baseline_mcmc function works as expected", {
  result <- baseline_mcmc()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_baseline_mcmc", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_mcmc")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_mcmc")
  app$click("baseline_mcmc-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

