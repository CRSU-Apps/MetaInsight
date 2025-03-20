test_that("Check bayes_nodesplit function works as expected", {
  result <- bayes_nodesplit()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_nodesplit", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_nodesplit")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_nodesplit")
  app$click("bayes_nodesplit-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

