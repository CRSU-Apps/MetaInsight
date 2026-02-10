test_that("Check baseline_regression function works as expected", {
  result <- baseline_regression(fitted_baseline_model, configured_data_con)

  expect_is(result, "regression_data")
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
                                     configured_data_con,
                                     result,
                                     c("Gabapentinoids", "Ketamine"))

  expect_match(plot_result, "<svg")
})

test_that("baseline_regression and metaregression_plot produce errors for incorrect data types", {

  fake_reg <- list()
  class(fake_reg) <- "regression_data"

  expect_error(baseline_regression("faulty_model", configured_data_con), "model must be of class baseline_model")
  expect_error(baseline_regression(fitted_baseline_model, "not_data"), "configured_data must be of class configured_data")

  expect_error(metaregression_plot("faulty_model", configured_data_con, fake_reg, "Gabapentinoids"), "model must be an object created by baseline_model")
  expect_error(metaregression_plot(fitted_baseline_model, "not_data", fake_reg, "Gabapentinoids"), "configured_data must be of class configured_data")
  expect_error(metaregression_plot(fitted_baseline_model, configured_data_con, "fake_reg", "Gabapentinoids"), "regression_data must be an object created by baseline_regression")
  expect_error(metaregression_plot(fitted_baseline_model, configured_data_con, fake_reg, 123), "comparators must be of class character")

  expect_error(metaregression_plot(fitted_baseline_model, configured_data_con, fake_reg, c("Gabapentinoids", "Placebo")), "comparators cannot contain the reference treatment")
  expect_error(metaregression_plot(fitted_baseline_model, configured_data_con, fake_reg, c("Gabapentinoids", "Meth")), "comparators must be present in the configured data")
})

test_that("{shinytest2} recording: e2e_baseline_regression", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_regression", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_regression")
  # needed to avoid error
  app$set_inputs("baseline_regression-baseline-credible" = FALSE)
  app$click("baseline_regression-baseline-run")
  app$wait_for_value(input = "baseline_regression-complete")

  common <- app$get_value(export = "common")
  result <- common$baseline_regression

  expect_is(result, "regression_data")
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
  expect_match(plot$html, "<svg")

  test_plot_downloads(app, "baseline_regression-baseline", FALSE)

  app$stop()
})

