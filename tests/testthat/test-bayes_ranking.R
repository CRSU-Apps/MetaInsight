test_that("Check bayes_ranking function works as expected", {
  result <- bayes_ranking()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_bayes_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_ranking")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_ranking")
  app$click("bayes_ranking-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$upgraded_data))
})

