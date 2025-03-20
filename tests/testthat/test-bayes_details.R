test_that("Check bayes_details function works as expected", {
  result <- bayes_details()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_details")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_details")
  app$click("bayes_details-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

