test_that("Check covariate_model function works as expected", {

  # time to compare later
  start_time <- proc.time()
  result_1 <- covariate_model(configured_data_con, 50, "shared")
  end_time <- proc.time()
  elapsed_1 <- end_time - start_time

  expect_is(result_1, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "covariate_value",
                    "reference_treatment",
                    "comparator_names",
                    "a",
                    "sumresults",
                    "dic",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome",
                    "outcome_measure",
                    "mtcNetwork",
                    "effects",
                    "covariate_min",
                    "covariate_max"
                    ) %in% names(result_1)))

  expect_is(result_1$mtcResults, "mtc.result")
  expect_is(result_1$mtcRelEffects, "mtc.result")
  expect_is(result_1$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result_1$covariate_value, "numeric")
  expect_is(result_1$reference_treatment, "character")
  expect_is(result_1$comparator_names, "character")
  expect_is(result_1$a, "character")
  expect_is(result_1$sumresults, "summary.mtc.result")
  expect_is(result_1$dic, "data.frame")
  expect_is(result_1$cov_value_sentence, "character")
  expect_is(result_1$slopes, "numeric")
  expect_is(result_1$intercepts, "numeric")
  expect_is(result_1$outcome, "character")
  expect_is(result_1$outcome_measure, "character")
  expect_is(result_1$mtcNetwork, "mtc.network")
  expect_is(result_1$effects, "character")
  expect_is(result_1$covariate_min, "numeric")
  expect_is(result_1$covariate_max, "numeric")

  expect_equal(result_1$a, "random effect")
  expect_equal(result_1$cov_value_sentence, "Value for covariate age set at 50")
  expect_equal(result_1$outcome, "continuous")
  expect_equal(result_1$outcome_measure, "MD")
  expect_equal(result_1$effects, "random")
  expect_equal(result_1$covariate_value, 50)
  expect_equal(result_1$reference_treatment, "Placebo")
  expect_equal(result_1$comparator_names, c("Gabapentinoids", "Glucocorticoids", "Ketamine"))

  expected_mcmc_table <- data.frame(characteristic = c("Chains",
                                                       "Burn-in iterations",
                                                       "Sample iterations",
                                                       "Thinning factor"),
                                    value = c(4, 5000, 20000, 1))

  expect_equal(metainsight:::GetGemtcMcmcCharacteristics(result_1$mtcResults), expected_mcmc_table)

  # adjust the output for a different covariate value. This should take less time than for result
  start_time <- proc.time()
  result_2 <- covariate_model(configured_data_con, 55, "shared", result_1)
  end_time <- proc.time()
  elapsed_2 <- end_time - start_time
  expect_false(identical(remove_igraph(result_1), remove_igraph(result_2)))
  expect_gt(elapsed_1[3], elapsed_2[3])

  # adjust the output for a different regressor type
  result_3 <- covariate_model(configured_data_con, 55, "unrelated", result_1)
  expect_false(identical(remove_igraph(result_2), remove_igraph(result_3)))

  # refit the first to ensure reproducibility
  result_4 <- covariate_model(configured_data_con, 50, "shared")
  expect_true(identical(remove_igraph(result_1), remove_igraph(result_4)))

})

test_that("FindCovariateRanges() finds ranges for continuous long data", {
  load <- setup_load(file.path(test_data_dir, "Contribution_continuous_long_continuous_cov.csv"), outcome = "continuous")
  config <- setup_configure(load, "Paracetamol", "random", "MD", "good", 123)

  ranges <- FindCovariateRanges(
    connected_data = config$connected_data,
    treatment_df = config$treatments,
    reference_treatment = "Paracetamol",
    covariate_title = "covar.age"
  )

  expected_min <- c(
    "Ibuprofen" = 98,
    "A_stiff_drink" = 95,
    "Sleep" = 97,
    "Exercise" = NA
  )

  expected_max <- c(
    "Ibuprofen" = 99,
    "A_stiff_drink" = 99,
    "Sleep" = 98,
    "Exercise" = NA
  )

  expect_mapequal(!!expected_min, !!ranges$min)
  expect_mapequal(!!expected_max, !!ranges$max)
})

test_that("FindCovariateRanges() finds ranges for continuous wide data", {

  load <- setup_load(file.path(test_data_dir, "Contribution_continuous_wide_continuous_cov.csv"), outcome = "continuous")
  config <- setup_configure(load, "Paracetamol", "random", "MD", "good", 123)

  ranges <- FindCovariateRanges(
    connected_data = config$connected_data,
    treatment_df = config$treatments,
    reference_treatment = "Paracetamol",
    covariate_title = "covar.age"
  )

  expected_min <- c(
    "Ibuprofen" = 98,
    "A_stiff_drink" = 95,
    "Sleep" = 97,
    "Exercise" = NA
  )

  expected_max <- c(
    "Ibuprofen" = 99,
    "A_stiff_drink" = 99,
    "Sleep" = 98,
    "Exercise" = NA
  )

  expect_mapequal(!!expected_min, !!ranges$min)
  expect_mapequal(!!expected_max, !!ranges$max)
})

test_that("covariate_model produces errors for incorrect data types", {
  expect_error(covariate_model("not_data", 50, "shared"), "configured_data must be of class configured_data")
  expect_error(covariate_model(configured_data_con, "not_numeric", "shared"), "covariate_value must be of class numeric")
  expect_error(covariate_model(configured_data_con, 50, 123), "regressor_type must be of class character")

  invalid_outcome_measure <- configured_data_con
  invalid_outcome_measure$outcome_measure <- "SMD"
  expect_error(covariate_model(invalid_outcome_measure, 50, "shared"), "configured data must have an outcome_measure")
  expect_error(covariate_model(configured_data_con, 50, "not_shared"), "regressor_type must be")

  # when no covariate exists
  no_cov_load <- setup_load(file.path(test_data_dir, "Cont_long.csv"), "continuous")
  no_cov_con <- setup_configure(no_cov_load, "the Great", "random", "MD", "good", 123)
  expect_error(covariate_model(no_cov_con, 99, "shared"), "The data does not contain a covariate column")

  # when covariate_value is out of range
  expect_error(covariate_model(configured_data_con, 1, "shared"), "covariate_value must not be lower than the minimum")
  expect_error(covariate_model(configured_data_con, 1000, "shared"), "covariate_value must not be higher than the maximum")

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

  # check slider updates
  slider_label <- app$get_text("#covariate_model-covariate_value-label")
  expect_equal(slider_label, "Covariate value (age)")

  min_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-min")
  mean_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-single")
  max_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-max")

  expect_equal(min_value, "24")
  expect_equal(mean_value, "55")
  expect_equal(max_value, "75.5")

  common <- app$get_value(export = "common")
  result <- common$covariate_model

  expect_is(result, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "covariate_value",
                    "reference_treatment",
                    "comparator_names",
                    "a",
                    "sumresults",
                    "dic",
                    "cov_value_sentence",
                    "slopes",
                    "intercepts",
                    "outcome",
                    "outcome_measure",
                    "mtcNetwork",
                    "effects",
                    "covariate_min",
                    "covariate_max"
  ) %in% names(result)))

  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$covariate_value, "integer")
  expect_is(result$reference_treatment, "character")
  expect_is(result$comparator_names, "character")
  expect_is(result$a, "character")
  expect_is(result$sumresults, "summary.mtc.result")
  expect_is(result$dic, "data.frame")
  expect_is(result$cov_value_sentence, "character")
  expect_is(result$slopes, "numeric")
  expect_is(result$intercepts, "numeric")
  expect_is(result$outcome, "character")
  expect_is(result$outcome_measure, "character")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$effects, "character")
  expect_is(result$covariate_min, "numeric")
  expect_is(result$covariate_max, "numeric")

})

test_that("sliderinput updates for binary covariate", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_model", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_wide_binary_cov.csv"))
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")

  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_model")

  slider_label <- app$get_text("#covariate_model-covariate_value-label")
  expect_equal(slider_label, "Covariate value (handedness)")

  min_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-min")
  mean_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-single")
  max_value <- app$get_text(".form-group:has(#covariate_model-covariate_value) .irs-max")

  expect_equal(min_value, "0")
  expect_equal(mean_value, "0")
  expect_equal(max_value, "1")

  app$stop()
})
