test_that("Check maths_multiply function works as expected", {
  result <- maths_multiply()
  expect_true(is.null(result))
})

test_that("{shinytest2} recording: e2e_maths_multiply", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_maths_multiply")
  app$set_inputs(tabs = "maths")
  app$set_inputs(mathsSel = "maths_multiply")
  app$click("maths_multiply-run")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$maths_number_2))
})

