test_that("Check baseline_forest function works as expected", {
  result <- baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df, "Placebo")
  expect_match(result, "<svg")
})

test_that("Check baseline_forest function produces errors as expected", {
  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(baseline_forest(fitted_baseline_model, "not_a_dataframe", "Placebo", "title"), "treatment_df must be of class data.frame")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df, 123, "title"), "reference_treatment must be of class character")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df, "Placebo", 123), "title must be of class character")

  expect_error(baseline_forest("faulty_model", loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by baseline_model")
  expect_error(baseline_forest(list(a = 1), loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by baseline_model")
  expect_error(baseline_forest(faulty_model, loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by baseline_model")
})


test_that("{shinytest2} recording: e2e_baseline_forest", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_forest")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_forest")
  app$click("baseline_forest-run")

  app$wait_for_value(output = "baseline_forest-baseline-plot")
  plot <- app$get_value(output = "baseline_forest-baseline-plot")
  expect_match(plot$html, "<svg")

  test_plot_downloads(app, "baseline_forest-baseline", FALSE)

  app$stop()
})

