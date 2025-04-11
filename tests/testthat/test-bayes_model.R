test_that("Check bayes_model function works as expected", {
  result <- bayes_model()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_model")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_model")
  app$click("bayes_model-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

