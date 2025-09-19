test_that("Check covariate_comparison function works as expected", {
  result <- covariate_comparison()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_comparison", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_comparison")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_comparison")
  app$click("covariate_comparison-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

