test_that("Check covariate_ranking function works as expected", {
  result <- covariate_ranking()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_covariate_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_ranking")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_ranking")
  app$click("covariate_ranking-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

