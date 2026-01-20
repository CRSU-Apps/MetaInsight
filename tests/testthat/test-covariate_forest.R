test_that("Check covariate_forest function works as expected", {
  result <- covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", title = "title")
  expect_match(result, "<svg")

  x_min_result <- covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", -1, NULL, "title")
  expect_match(x_min_result, "<svg")

  x_max_result <- covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", NULL, 1000, "title")
  expect_match(x_max_result, "<svg")

  expect_false(identical(result, x_min_result))
  expect_false(identical(result, x_max_result))
  expect_false(identical(x_min_result, x_max_result))

})

test_that("Check covariate_forest function produces errors as expected", {
  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(covariate_forest(fitted_covariate_model, "not_a_dataframe", "Placebo", 1, 2, "title"), "treatment_df must be of class data.frame")
  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, 123, 1, 2, "title"), "reference_treatment must be of class character")
  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", "1", 2, "title"), "xmin must be of class numeric")
  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", 1, "2", "title"), "xmax must be of class numeric")
  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", 1, 2, 123), "title must be of class character")

  expect_error(covariate_forest("faulty_model", loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by bayes_model")
  expect_error(covariate_forest(list(a = 1), loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by bayes_model")
  expect_error(covariate_forest(faulty_model, loaded_data_con$treatment_df, "Placebo", "title"), "model must be an object created by bayes_model")

  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", 3, 2, "title"), "xmin must be less than xmax")
  expect_error(covariate_forest(fitted_covariate_model, loaded_data_con$treatment_df, "Placebo", 3, 3, "title"), "xmin must be less than xmax")
})

test_that("{shinytest2} recording: e2e_covariate_forest", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_forest")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_forest")
  app$click("covariate_forest-run")

  app$wait_for_value(output = "covariate_forest-covariate-plot")
  plot <- app$get_value(output = "covariate_forest-covariate-plot")
  expect_match(plot$html, "<svg")

  test_plot_downloads(app, "covariate_forest-covariate", FALSE)

  app$set_inputs("covariate_forest-xmax" = 1)
  plot_updated <- app$get_value(output = "covariate_forest-covariate-plot")

  expect_false(identical(plot, plot_updated))

  app$stop()
})

