test_that("Check bayes_forest function works as expected", {
  test_file <- tempfile(fileext = ".png")

  png(test_file)
  bayes_forest(fitted_bayes_model)
  dev.off()

  expect_true(file.exists(test_file))
  expect_gt(file.size(test_file), 1000)
  unlink(test_file)
})

test_that("Check bayes_forest function produces errors as expected", {
  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(bayes_forest("faulty_model"), "model must be an object created by bayes_model")
  expect_error(bayes_forest(list(a = 1)), "model must be an object created by bayes_model")
  expect_error(bayes_forest(faulty_model), "model must be an object created by bayes_model")
})

test_that("{shinytest2} recording: e2e_bayes_forest", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_forest", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_forest")
  app$click("bayes_forest-run")

  app$wait_for_value(output = "bayes_forest-all-plot")
  app$wait_for_value(output = "bayes_forest-sub-plot")

  plot_all <- app$get_value(output = "bayes_forest-all-plot")
  plot_sub <- app$get_value(output = "bayes_forest-sub-plot")

  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")

  test_bayes_plot_downloads(app, "bayes_forest", "")
})

