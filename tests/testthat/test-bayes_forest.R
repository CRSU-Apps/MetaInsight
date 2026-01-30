test_that("Check bayes_forest function works as expected", {
  result <- bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, NULL, NULL, "title")
  expect_match(result, "<svg")

  x_min_result <- bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, -1, NULL, "title")
  expect_match(x_min_result, "<svg")

  x_max_result <- bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, NULL, 1000, "title")
  expect_match(x_max_result, "<svg")

  expect_false(identical(result, x_min_result))
  expect_false(identical(result, x_max_result))
  expect_false(identical(x_min_result, x_max_result))
})

test_that("Check bayes_forest function produces errors as expected", {
  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(bayes_forest(fitted_bayes_model, "not_a_dataframe", 1, 2, "title"), "treatment_df must be of class data.frame")
  expect_error(bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, "1", 2, "title"), "xmin must be of class numeric")
  expect_error(bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, 1, "2", "title"), "xmax must be of class numeric")
  expect_error(bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, 1, 2, 123), "title must be of class character")

  expect_error(bayes_forest("faulty_model", loaded_data_con$treatment_df, 1, 2, "title"), "model must be an object created by bayes_model")
  expect_error(bayes_forest(list(a = 1), loaded_data_con$treatment_df, 1, 2, "title"), "model must be an object created by bayes_model")
  expect_error(bayes_forest(faulty_model, loaded_data_con$treatment_df, 1, 2, "title"), "model must be an object created by bayes_model")

  expect_error(bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, 3, 2, "title"), "xmin must be less than xmax")
  expect_error(bayes_forest(fitted_bayes_model, loaded_data_con$treatment_df, 3, 3, "title"), "xmin must be less than xmax")
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

  app$set_inputs("bayes_forest-xmin_all" = 1)
  plot_all_updated <- app$get_value(output = "bayes_forest-all-plot")

  expect_false(identical(plot_all, plot_all_updated))

  app$stop()
})

