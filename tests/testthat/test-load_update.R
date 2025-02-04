test_that("Check setup_upgrade function works as expected", {
  result <- setup_upgrade()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_setup_upgrade", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_upgrade")
  app$set_inputs(tabs = "load")
  app$set_inputs(loadSel = "setup_upgrade")
  app$click("setup_upgrade-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$continuous_file))
})

