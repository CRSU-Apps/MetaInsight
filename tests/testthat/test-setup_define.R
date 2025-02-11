mock_data <- read.csv("data/Cont_long_continuous_cov.csv")
mock_treatments <- unique(mock_data$T)
mock_treatment_df <- data.frame(1:length(mock_treatments), mock_treatments)
colnames(mock_treatment_df) <- c("Number", "Label")

mock_data_bin <- read.csv("data/Cont_long_binary_cov.csv")
mock_treatments_bin <- unique(mock_data$T)
mock_treatment_df_bin <- data.frame(1:length(mock_treatments), mock_treatments)
colnames(mock_treatment_df_bin) <- c("Number", "Label")

test_that("setup_upgrade returns errors for faulty inputs", {
  expect_error(setup_define("not_a_dataframe", mock_treatment_df, "Continuous", "OR", "the Great"), "data must be of class data.frame")
  expect_error(setup_define(mock_data, "not_a_dataframe", "Continuous", "OR", "the Great"), "treatment_df must be of class data.frame")
  expect_error(setup_define(mock_data, mock_treatment_df, 123, "OR", "the Great"), "outcome must be of class character")
  expect_error(setup_define(mock_data, mock_treatment_df, "Continuous", 123, "the Great"), "outcome_measure must be of class character")
  expect_error(setup_define(mock_data, mock_treatment_df, "Continuous", "OR", 123), "reference_treatment must be of class character")
  expect_error(setup_define(mock_data, mock_treatment_df, "InvalidOutcome", "OR", "the Great"), "outcome must be either Binary or Continuous")
  expect_error(setup_define(mock_data, mock_treatment_df, "Continuous", "InvalidMeasure", "the Great"), "outcome_measure must be either MD or SMD")
  expect_error(setup_define(mock_data_bin, mock_treatment_df_bin, "Binary", "InvalidMeasure", "the Great"), "outcome_measure must be either OR, RR or RD")
})

test_that("setup_define returns correctly structured objects", {

  expected_items <- c("wrangled_data", "treatment_df", "disconnected_indices", "main_connected_data",
    "initial_non_covariate_data", "bugsnet_all", "freq_all")

  result <- setup_define(mock_data, mock_treatment_df, "Continuous", "MD", "the Great")
  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))

  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatment_df, "data.frame")
  expect_length(result$disconnected_indices, 0)
  expect_s3_class(result$main_connected_data, "data.frame")
  expect_s3_class(result$initial_non_covariate_data, "data.frame")
  expect_s3_class(result$bugsnet_all, "data.frame")
  expect_type(result$freq_all, "list")
})

test_that("setup_define loads data into common correctly for continuous long data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "load")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = "data/Cont_long_continuous_cov.csv")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$initial_non_covariate_data, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment, "the Great")
  expect_equal(common$outcome_measure, "MD")
})


test_that("setup_define loads data into common correctly for wide binary data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "load")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = system.file("extdata", "binary_wide.csv", package = "metainsight"))
  app$set_inputs("setup_load-outcome" = "Binary")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$initial_non_covariate_data, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment, "Placebo")
  expect_equal(common$outcome_measure, "OR")
})


test_that("setup_define logs errors when disconnected data is uploaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "load")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = "data/continuous_long_disconnected.csv")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_type(common$disconnected_indices, "integer")
  expect_gt(length(common$disconnected_indices), 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$initial_non_covariate_data, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment, "A")
  expect_equal(common$outcome_measure, "MD")

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*The uploaded data comprises a disconnected network*", logger))
})

