test_that("Check covariate_details function works as expected", {
  result <- covariate_details()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_details")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_details")
  app$click("covariate_details-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

