test_that("Check summary_study function works as expected", {
  result <- summary_study()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_summary_study", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_summary_study")
  app$set_inputs(tabs = "summary")
  app$set_inputs(summarySel = "summary_study")
  app$click("summary_study-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$continuous_file))
})

