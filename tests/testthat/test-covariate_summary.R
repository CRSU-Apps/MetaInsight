test_that("Check covariate_summary function works as expected", {
  result <- covariate_summary()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_summary", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_summary")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_summary")
  app$click("covariate_summary-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

