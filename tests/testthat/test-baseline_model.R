test_that("Check baseline_model function works as expected", {
  result <- baseline_model(defined_data_con$main_connected_data, loaded_data_con$treatment_df, "Continuous", "MD", "Placebo", "random", "shared", 123)

  expect_is(result, "baseline_model")
  expect_true(all(c("mtcResults",
                    "covariate_value",
                    "reference_name",
                    "comparator_names",
                    "a",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome",
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
  expect_is(result$outcome, "character")
  expect_is(result$model, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
  expect_is(result$dic, "data.frame")
  expect_is(result$sumresults, "summary.network.result")
  expect_is(result$regressor, "character")

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
                    "outcome",
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
  expect_is(result$outcome, "character")
  expect_is(result$model, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
  expect_is(result$dic, "data.frame")
  expect_is(result$sumresults, "summary.network.result")
  expect_is(result$regressor, "character")

  app$stop()
})

