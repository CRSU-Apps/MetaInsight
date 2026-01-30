test_that("Check baseline_forest function works as expected", {
  result <- baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df, title = "title")
  expect_match(result, "<svg")

  x_min_result <- baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  -1, NULL, "title")
  expect_match(x_min_result, "<svg")

  x_max_result <- baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  NULL, 1000, "title")
  expect_match(x_max_result, "<svg")

  expect_false(identical(result, x_min_result))
  expect_false(identical(result, x_max_result))
  expect_false(identical(x_min_result, x_max_result))

})

test_that("Check baseline_forest function produces errors as expected", {
  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(baseline_forest(fitted_baseline_model, "not_a_dataframe",  1, 2, "title"), "treatment_df must be of class data.frame")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  "1", 2, "title"), "xmin must be of class numeric")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  1, "2", "title"), "xmax must be of class numeric")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  1, 2, 123), "title must be of class character")

  expect_error(baseline_forest("faulty_model", loaded_data_con$treatment_df,  "title"), "model must be an object created by baseline_model")
  expect_error(baseline_forest(list(a = 1), loaded_data_con$treatment_df,  "title"), "model must be an object created by baseline_model")
  expect_error(baseline_forest(faulty_model, loaded_data_con$treatment_df,  "title"), "model must be an object created by baseline_model")

  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  3, 2, "title"), "xmin must be less than xmax")
  expect_error(baseline_forest(fitted_baseline_model, loaded_data_con$treatment_df,  3, 3, "title"), "xmin must be less than xmax")
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

