test_that("Check covariate_results function works as expected", {
  result <- covariate_results()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_results", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_results")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_results")
  app$click("covariate_results-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

