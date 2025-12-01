connected <- defined_data_con$main_connected_data
t_df <- defined_data_con$treatment_df

test_that("Check baseline_model function works as expected", {
  result <- baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "random", "shared", 123)

  expect_is(result, "baseline_model")
  expect_true(all(c("mtcResults",
                    "covariate_value",
                    "reference_name",
                    "comparator_names",
                    "a",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome_measure",
                    "model",
                    "covariate_min",
                    "covariate_max",
                    "dic",
                    "sumresults",
                    "regressor") %in% names(result)))
  expect_is(result$mtcResults, "network.result")
  expect_is(result$covariate_value, "numeric")
  expect_is(result$reference_name, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome_measure, "character")
  expect_is(result$model, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
  expect_is(result$dic, "data.frame")
  expect_is(result$sumresults, "summary.network.result")
  expect_is(result$regressor, "character")

  # check results are reproducible
  result_2 <- baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "random", "shared", 123)
  expect_true(identical(result, result_2))

})

test_that("baseline_model produces errors for incorrect data types", {
  expect_error(baseline_model("not_a_dataframe", t_df, "Continuous", "MD", "Placebo", "random", "shared", 999), "connected_data must be of class data.frame")
  expect_error(baseline_model(connected, "not_a_dataframe", "Continuous", "MD", "Placebo", "random", "shared", 999), "treatment_df must be of class data.frame")
  expect_error(baseline_model(connected, t_df, 123, "MD", "Placebo", "random", "shared", 999), "outcome must be of class character")
  expect_error(baseline_model(connected, t_df, "Continuous", 123, "Placebo", "random", "shared", 999), "outcome_measure must be of class character")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", 123, "random", "shared", 999), "reference_treatment must be of class character")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "Placebo", 123, "shared", 999), "model_type must be of class character")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "random", 123, 999), "regressor_type must be of class character")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "random", "shared", "seed"), "seed must be of class numeric")

  expect_error(baseline_model(connected, t_df, "invalid_outcome", "MD", "Placebo", "random", "shared", 999), "outcome must be 'Binary' or 'Continuous'")
  expect_error(baseline_model(connected, t_df, "Continuous", "invalid_measure", "Placebo", "random", "shared", 999), "outcome_measure must be 'OR', 'RR' or 'MD'")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "not_random", "shared", 999), "model_type must be 'fixed' or 'random'")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "not_a_placebo", "random", "shared", 999), "reference_treatment must be one of the treatments in treatment_df")
  expect_error(baseline_model(connected, t_df, "Continuous", "MD", "Placebo", "random", "not_shared", 999), "regressor_type must be")
})


test_that("{shinytest2} recording: e2e_baseline_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_model", timeout = 30000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_model")
  app$click("baseline_model-run")
  app$wait_for_value(input = "baseline_model-complete")
  common <- app$get_value(export = "common")

  result <- common$baseline_model

  expect_is(result, "baseline_model")
  expect_true(all(c("mtcResults",
                    "covariate_value",
                    "reference_name",
                    "comparator_names",
                    "a",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome_measure",
                    "model",
                    "covariate_min",
                    "covariate_max",
                    "dic",
                    "sumresults",
                    "regressor") %in% names(result)))
  expect_is(result$mtcResults, "network.result")
  expect_is(result$covariate_value, "numeric")
  expect_is(result$reference_name, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome_measure, "character")
  expect_is(result$model, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
  expect_is(result$dic, "data.frame")
  expect_is(result$sumresults, "summary.network.result")
  expect_is(result$regressor, "character")

  app$stop()
})

