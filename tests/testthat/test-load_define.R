test_that("Check load_define function works as expected", {
  result <- load_define()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_load_define", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_load_define")
  app$set_inputs(tabs = "load")
  app$set_inputs(loadSel = "load_define")
  app$click("load_define-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$continuous_file))
})

