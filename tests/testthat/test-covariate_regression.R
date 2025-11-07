test_that("Check covariate_regression function works as expected", {
  result <- covariate_regression(fitted_covariate_model,
                                defined_data_con$main_connected_data,
                                "covar.age",
                                defined_data_con$treatment_df,
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


  plot_result <- metaregression_plot(fitted_covariate_model,
                                     defined_data_con$treatment_df,
                                     "MD",
                                     c("Gabapentinoids", "Ketamine"),
                                     result$directness,
                                     result$credible_regions)

  expect_is(plot_result, "ggplot")

})

test_that("{shinytest2} recording: e2e_covariate_regression", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_regression", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_regression")
  # needed to avoid error
  app$set_inputs("covariate_regression-covariate-credible" = FALSE)
  app$click("covariate_regression-covariate-run")
  app$wait_for_value(input = "covariate_regression-complete")

  common <- app$get_value(export = "common")
  result <- common$covariate_regression

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

  app$wait_for_value(output = "covariate_regression-covariate-plot")
  plot <- app$get_value(output = "covariate_regression-covariate-plot")
  expect_equal(substr(plot$src, 1, 10), "data:image")

  test_plot_downloads(app, "covariate_regression-covariate", FALSE)

  app$stop()
})

