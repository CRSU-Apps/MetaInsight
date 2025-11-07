connected <- defined_data_con$main_connected_data
t_df <- defined_data_con$treatment_df

test_that("Check covariate_model function works as expected", {
  result <- covariate_model(connected, t_df, "Continuous", "MD", 50, "random", "shared", "Placebo", 123)

  expect_is(result, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "covariate_value",
                    "reference_name",
                    "comparator_names",
                    "a",
                    "sumresults",
                    "dic",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome_measure",
                    "mtcNetwork",
                    "model_type",
                    "covariate_min",
                    "covariate_max"
                    ) %in% names(result)))

  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$covariate_value, "numeric")
  expect_is(result$reference_name, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$sumresults, "summary.mtc.result")
  expect_is(result$dic, "data.frame")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome_measure, "character")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$model_type, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
})

test_that("covariate_model produces errors for incorrect data types", {
  expect_error(covariate_model("not_a_dataframe", t_df, "Continuous", "MD", 50, "random", "shared", "Placebo", 999), "connected_data must be of class data.frame")
  expect_error(covariate_model(connected, "not_a_dataframe", "Continuous", "MD", 50, "random", "shared", "Placebo", 999), "treatment_df must be of class data.frame")
  expect_error(covariate_model(connected, t_df, 123, "MD", 50, "random", "shared", "Placebo", 999), "outcome must be of class character")
  expect_error(covariate_model(connected, t_df, "Continuous", 123, 50, "random", "shared", "Placebo", 999), "outcome_measure must be of class character")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", "not_numeric", "random", "shared", "Placebo", 999), "cov_value must be of class numeric")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, 123, "shared", "Placebo", 999), "model_type must be of class character")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "random", 123, "Placebo", 999), "regressor_type must be of class character")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "random", "shared", 123, 999), "reference_treatment must be of class character")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "random", "shared", "Placebo", "not_seed"), "seed must be of class numeric")

  # when no covariate exists
  no_cov_load <- setup_load(file.path(test_data_dir, "Cont_long.csv"), "Continuous")
  no_cov_con <- setup_configure(no_cov_load$data, no_cov_load$treatment_df, "Continuous", "MD", "the_Great")
  expect_error(covariate_model(no_cov_con$main_connected_data, no_cov_con$treatment_df, "Continuous", "MD", 99, "random", "shared", "the_Great", 999), "connected_data does not contain a covariate column")

  # when covariate_value is out of range
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 1, "random", "shared", "Placebo", 999), "cov_value must not be lower than the minimum")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 1000, "random", "shared", "Placebo", 999), "cov_value must not be higher than the maximum")

  expect_error(covariate_model(connected, t_df, "invalid_outcome", "MD", 50, "random", "shared", "Placebo", 999), "outcome must be 'Binary' or 'Continuous'")
  expect_error(covariate_model(connected, t_df, "Continuous", "invalid_measure", 50, "random", "shared", "Placebo", 999), "outcome_measure must be 'OR', 'RR' or 'MD'")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "not_random", "shared", "Placebo", 999), "model_type must be 'fixed' or 'random'")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "random", "not_shared", "Placebo", 999), "regressor_type must be")
  expect_error(covariate_model(connected, t_df, "Continuous", "MD", 50, "random", "shared", "not_placebo", 999), "reference_treatment must be one of the treatments in treatment_df")

})

test_that("{shinytest2} recording: e2e_covariate_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_model", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")

  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_model")
  app$click("covariate_model-run")
  app$wait_for_value(input = "covariate_model-complete")
  common <- app$get_value(export = "common")

  result <- common$covariate_model

  expect_is(result, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "covariate_value",
                    "reference_name",
                    "comparator_names",
                    "a",
                    "sumresults",
                    "dic",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome_measure",
                    "mtcNetwork",
                    "model_type",
                    "covariate_min",
                    "covariate_max"
  ) %in% names(result)))

  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$covariate_value, "numeric")
  expect_is(result$reference_name, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$sumresults, "summary.mtc.result")
  expect_is(result$dic, "data.frame")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome_measure, "character")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$model_type, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")

})

