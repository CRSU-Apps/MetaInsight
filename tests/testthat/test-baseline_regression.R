test_that("Check baseline_regression function works as expected", {
  result <- baseline_regression(fitted_baseline_model,
                                defined_data_con$main_connected_data,
                                "covar.baseline_risk",
                                loaded_data_con$treatment_df,
                                "Continuous",
                                "MD",
                                "random")

  expect_is(result, "list")
  expect_true(all(c("directness",
                    "credible_regions") %in% names(result)))
  expect_is(result$directness, "list")
  expect_is(result$credible_regions, "list")

  expect_true(all(c("is_direct",
                    "is_indirect",
                    "relative_effect",
                    "covariate_value") %in% names(result$directness)))

  expect_true(all(c("regions",
                    "intervals") %in% names(result$credible_regions)))


  plot_result <- metaregression_plot(fitted_baseline_model,
                                     loaded_data_con$treatment_df,
                                     "MD",
                                     "Ketamine",
                                     result$directness,
                                     result$credible_regions)

  expect_is(plot_result, "ggplot")

})

test_that("{shinytest2} recording: e2e_baseline_regression", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_mcmc")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_regression")
  app$click("baseline_regression-baseline-run")
  app$wait_for_value(input = "baseline_regression-complete")

  common <- app$get_value(export = "common")
  result <- common$baseline_regression

  expect_is(result, "list")
  expect_true(all(c("directness",
                    "credible_regions") %in% names(result)))
  expect_is(result$directness, "list")
  expect_is(result$credible_regions, "list")

  expect_true(all(c("is_direct",
                    "is_indirect",
                    "relative_effect",
                    "covariate_value") %in% names(result$directness)))

  expect_true(all(c("regions",
                    "intervals") %in% names(result$credible_regions)))

  app$wait_for_value(output = "baseline_regression-baseline-plot")
  plot <- app$get_value(output = "baseline_regression-baseline-plot")
  expect_equal(substr(plot$src, 1, 10), "data:image")

  test_plot_downloads(app, "baseline_regression-baseline", FALSE)

  app$stop()
})

