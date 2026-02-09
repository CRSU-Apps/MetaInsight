
test_that("Check baseline_model function works as expected", {
  result <- baseline_model(configured_data_con, "shared")

  expect_is(result, "baseline_model")

  expected_items <- c("mtcResults",
  "covariate_value",
  "reference_treatment",
  "comparator_names",
  "a",
  "cov_value_sentence",
  "slopes",
  "intercepts",
  "outcome",
  "outcome_measure",
  "effects",
  "covariate_min",
  "covariate_max",
  "dic",
  "sumresults",
  "regressor")

  expect_true(all(c(expected_items) %in% names(result)))
  expect_is(result$mtcResults, "network.result")
  expect_is(result$covariate_value, "numeric")
  expect_is(result$reference_treatment, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome, "character")
  expect_is(result$outcome_measure, "character")
  expect_is(result$effects, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")
  expect_is(result$dic, "data.frame")
  expect_is(result$sumresults, "summary.network.result")
  expect_is(result$regressor, "character")

  # check results are reproducible
  result_2 <- baseline_model(configured_data_con, "shared")
  expect_true(identical(result, result_2))

  # check for binary data
  result_3 <- baseline_model(configured_data_bin, "shared")
  expect_true(all(c(expected_items) %in% names(result_3)))
})

test_that("baseline_model produces errors for incorrect data types", {
  expect_error(baseline_model("not_data", "shared"), "configured_data must be of class configured_data")
  expect_error(baseline_model(configured_data_con, 123), "regressor_type must be of class character")
  expect_error(baseline_model(configured_data_con, "not_shared"), "regressor_type must be")

  invalid_outcome_measure <- configured_data_con
  invalid_outcome_measure$outcome_measure <- "SMD"
  expect_error(baseline_model(invalid_outcome_measure, "shared"), "configured data must have an outcome_measure")

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
                    "reference_treatment",
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
  expect_is(result$reference_treatment, "character")
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

