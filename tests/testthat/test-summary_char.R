test_that("Check summary_char function works as expected", {
  result <- summary_char()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_summary_char", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_summary_char")
  app$set_inputs(tabs = "summary")
  app$set_inputs(summarySel = "summary_char")
  app$click("summary_char-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$continuous_file))
})

