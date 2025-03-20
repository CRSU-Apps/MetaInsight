test_that("Check bayes_results function works as expected", {
  result <- bayes_results()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_results", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_results")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_results")
  app$click("bayes_results-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

