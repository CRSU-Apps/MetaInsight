test_that("Check bayes_compare function works as expected", {
  result <- bayes_compare()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_compare", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_compare")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_compare")
  app$click("bayes_compare-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

