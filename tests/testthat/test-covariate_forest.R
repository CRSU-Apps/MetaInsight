test_that("Check covariate_forest function works as expected", {
  result <- covariate_forest()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_forest", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_forest")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_forest")
  app$click("covariate_forest-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

