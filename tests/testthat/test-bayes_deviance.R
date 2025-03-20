test_that("Check bayes_deviance function works as expected", {
  result <- bayes_deviance()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_deviance", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_deviance")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_deviance")
  app$click("bayes_deviance-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

