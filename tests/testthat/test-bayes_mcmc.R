test_that("Check bayes_mcmc function works as expected", {
  result <- bayes_mcmc()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_mcmc", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_mcmc")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_mcmc")
  app$click("bayes_mcmc-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

