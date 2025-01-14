test_that("Check summary_network function works as expected", {
  result <- summary_network()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_summary_network", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_summary_network")
  app$set_inputs(tabs = "summary")
  app$set_inputs(summarySel = "summary_network")
  app$click("summary_network-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$continuous_file))
})

