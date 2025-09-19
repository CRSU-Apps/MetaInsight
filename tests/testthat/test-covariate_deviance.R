test_that("Check covariate_deviance function works as expected", {
  result <- covariate_deviance()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_deviance", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_deviance")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_deviance")
  app$click("covariate_deviance-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

