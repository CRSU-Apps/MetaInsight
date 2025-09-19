test_that("Check covariate_model function works as expected", {
  result <- covariate_model()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_model")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_model")
  app$click("covariate_model-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

