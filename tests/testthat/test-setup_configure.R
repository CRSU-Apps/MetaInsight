mock_data <- read.csv("data/Cont_long_continuous_cov.csv")
mock_treatments <- unique(mock_data$T)
mock_treatment_df <- data.frame(1:length(mock_treatments), mock_treatments)
colnames(mock_treatment_df) <- c("Number", "Label")

mock_data_bin <- read.csv("data/Cont_long_binary_cov.csv")
mock_treatments_bin <- unique(mock_data$T)
mock_treatment_df_bin <- data.frame(1:length(mock_treatments), mock_treatments)
colnames(mock_treatment_df_bin) <- c("Number", "Label")

test_that("setup_configure returns errors for faulty inputs", {
  expect_error(setup_configure("not_a_dataframe", mock_treatment_df, "Continuous", "OR", "the Great"), "data must be of class data.frame")
  expect_error(setup_configure(mock_data, "not_a_dataframe", "Continuous", "OR", "the Great"), "treatment_df must be of class data.frame")
  expect_error(setup_configure(mock_data, mock_treatment_df, 123, "OR", "the Great"), "outcome must be of class character")
  expect_error(setup_configure(mock_data, mock_treatment_df, "Continuous", 123, "the Great"), "outcome_measure must be of class character")
  expect_error(setup_configure(mock_data, mock_treatment_df, "Continuous", "OR", 123), "reference_treatment must be of class character")
  expect_error(setup_configure(mock_data, mock_treatment_df, "InvalidOutcome", "OR", "the Great"), "outcome must be either Binary or Continuous")
  expect_error(setup_configure(mock_data, mock_treatment_df, "Continuous", "InvalidMeasure", "the Great"), "outcome_measure must be either MD or SMD")
  expect_error(setup_configure(mock_data_bin, mock_treatment_df_bin, "Binary", "InvalidMeasure", "the Great"), "outcome_measure must be either OR, RR or RD")
})

test_that("setup_configure returns correctly structured objects", {

  expected_items <- c("wrangled_data", "treatment_df", "disconnected_indices", "main_connected_data",
    "non_covariate_data_all", "covariate_column", "covariate_name", "covariate_type", "bugsnet_all", "freq_all")

  result <- setup_configure(mock_data, mock_treatment_df, "Continuous", "MD", "the Great")
  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))

  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatment_df, "data.frame")
  expect_length(result$disconnected_indices, 0)
  expect_s3_class(result$main_connected_data, "data.frame")
  expect_s3_class(result$non_covariate_data_all, "data.frame")
  expect_type(result$covariate_column, "character")
  expect_type(result$covariate_name, "character")
  expect_type(result$covariate_type, "character")
  expect_s3_class(result$bugsnet_all, "data.frame")
  expect_type(result$freq_all, "list")
})

test_that("setup_configure loads data into common correctly for continuous long data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$non_covariate_data_all, "data.frame")
  expect_type(common$covariate_column, "character")
  expect_type(common$covariate_name, "character")
  expect_type(common$covariate_type, "character")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment_all, "the_Great")
  expect_equal(common$outcome_measure, "MD")

  app$stop()
})

test_that("setup_configure loads data into common correctly for wide binary data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = system.file("extdata", "binary_wide.csv", package = "metainsight"))
  app$set_inputs("setup_load-outcome" = "Binary")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$non_covariate_data_all, "data.frame")
  expect_type(common$covariate_column, "character")
  expect_type(common$covariate_name, "character")
  expect_type(common$covariate_type, "character")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment_all, "Placebo")
  expect_equal(common$outcome_measure, "OR")

  app$stop()
})

test_that("setup_configure logs errors when disconnected data is uploaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = "data/continuous_long_disconnected.csv")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_type(common$disconnected_indices, "integer")
  expect_gt(length(common$disconnected_indices), 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$non_covariate_data_all, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment_all, "A")
  expect_equal(common$outcome_measure, "MD")

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*The uploaded data comprises a disconnected network*", logger))

  app$stop()
})

test_that("Data wrangled from default continuous long file", {

  load <- setup_load(outcome = "Continuous")
  config <- setup_configure(load$data, load$treatment_df, "Continuous", "MD", "Placebo")

  expect_equal(colnames(config$wrangled_data), c("StudyID", colnames(load$data)[c(1, 2, 5, 3, 4, 7:15, 6)]),
               label = format_vector_to_string(colnames(config$wrangled_data)))

  expect_equal(config$wrangled_data$StudyID, rep(1:45, each = 2),
               label = format_vector_to_string(config$wrangled_data$StudyID))
  expect_equal(config$wrangled_data$T, c(rep(c(1, 2), times = 4), rep(c(1, 3), times = 18), rep(c(1, 4), times = 23)),
               label = format_vector_to_string(config$wrangled_data$T))

  expect_equal(nrow(config$wrangled_data), nrow(load$data),
               label = nrow(config$wrangled_data),
               expected.label = nrow(load$data))
  expect_equal(config$wrangled_data$Study, load$data$Study,
               label = format_vector_to_string(config$wrangled_data$Study),
               expected.label = format_vector_to_string(load$data$Study))
  expect_equal(config$wrangled_data$N, load$data$N,
               label = format_vector_to_string(config$wrangled_data$N),
               expected.label = format_vector_to_string(load$data$N))
  expect_equal(config$wrangled_data$Mean, load$data$Mean,
               label = format_vector_to_string(config$wrangled_data$Mean),
               expected.label = format_vector_to_string(load$data$Mean))
  expect_equal(config$wrangled_data$SD, load$data$SD,
               label = format_vector_to_string(config$wrangled_data$SD),
               expected.label = format_vector_to_string(load$data$SD))
})

test_that("Continuous wide data wrangled with treatment IDs", {

  load <- setup_load(file.path(test_data_dir, "Non_opioids_wide.csv"), outcome = "Continuous")
  config <- setup_configure(load$data, load$treatment_df, "Continuous", "MD", "Placebo")

  expect_equal(config$wrangled_data$StudyID, 1:45,
               label = format_vector_to_string(config$wrangled_data$StudyID))
  expect_equal(config$wrangled_data$T.1, c(rep(1, times = 45)),
               label = format_vector_to_string(config$wrangled_data$T.1))
  expect_equal(config$wrangled_data$T.2, c(rep(2, times = 4), rep(3, times = 18), rep(4, times = 23)),
               label = format_vector_to_string(config$wrangled_data$T.2))

  expect_equal(nrow(config$wrangled_data), nrow(load$data),
               label = nrow(config$wrangled_data),
               expected.label = nrow(load$data))
  expect_equal(config$wrangled_data$Study, load$data$Study,
               label = format_vector_to_string(config$wrangled_data$Study),
               expected.label = format_vector_to_string(load$data$Study))
  expect_equal(config$wrangled_data$N.1, load$data$N.1,
               label = format_vector_to_string(config$wrangled_data$N.1),
               expected.label = format_vector_to_string(load$data$N.1))
  expect_equal(config$wrangled_data$Mean.1, load$data$Mean.1,
               label = format_vector_to_string(config$wrangled_data$Mean.1),
               expected.label = format_vector_to_string(load$data$Mean.1))
  expect_equal(config$wrangled_data$SD.1, load$data$SD.1,
               label = format_vector_to_string(config$wrangled_data$SD.1),
               expected.label = format_vector_to_string(load$data$SD.1))
  expect_equal(config$wrangled_data$N.2, load$data$N.2,
               label = format_vector_to_string(config$wrangled_data$N.2),
               expected.label = format_vector_to_string(load$data$N.2))
  expect_equal(config$wrangled_data$Mean.2, load$data$Mean.2,
               label = format_vector_to_string(config$wrangled_data$Mean.2),
               expected.label = format_vector_to_string(load$data$Mean.2))
  expect_equal(config$wrangled_data$SD.2, load$data$SD.2,
               label = format_vector_to_string(config$wrangled_data$SD.2),
               expected.label = format_vector_to_string(load$data$SD.2))
})

test_that("Data wrangled from default binary long file", {

  load <- setup_load(outcome = "Binary")
  config <- setup_configure(load$data, load$treatment_df, "Binary", "OR", "Placebo")

  expect_equal(colnames(config$wrangled_data), c("StudyID", colnames(load$data)[c(1, 2, 4, 3, 5)]),
               label = format_vector_to_string(colnames(config$wrangled_data)))

  expect_equal(config$wrangled_data$StudyID, rep(1:12, each = 2),
               label = format_vector_to_string(config$wrangled_data$StudyID))
  expect_equal(config$wrangled_data$T, c(1, 2, 1, 3, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, 1, 5, 1, 5, 1, 2, 1, 6, 1, 7),
               label = format_vector_to_string(config$wrangled_data$T))

  expect_equal(nrow(config$wrangled_data), nrow(load$data),
               label = nrow(config$wrangled_data),
               expected.label = nrow(load$data))
  expect_equal(config$wrangled_data$Study, load$data$Study,
               label = format_vector_to_string(config$wrangled_data$Study),
               expected.label = format_vector_to_string(load$data$Study))
  expect_equal(config$wrangled_data$R, load$data$R,
               label = format_vector_to_string(config$wrangled_data$R),
               expected.label = format_vector_to_string(load$data$R))
  expect_equal(config$wrangled_data$N, load$data$N,
               label = format_vector_to_string(config$wrangled_data$N),
               expected.label = format_vector_to_string(load$data$N))
})

test_that("Binary wide data wrangled with treatment IDs", {

  load <- setup_load(file.path(test_data_dir, "Certolizumab_wide.csv"), outcome = "Binary")
  config <- setup_configure(load$data, load$treatment_df, "Binary", "OR", "Placebo")

  expect_equal(config$wrangled_data$StudyID, 1:12,
               label = format_vector_to_string(config$wrangled_data$StudyID))
  expect_equal(config$wrangled_data$T.1, rep(1, times = 12),
               label = format_vector_to_string(config$wrangled_data$T.1))
  expect_equal(config$wrangled_data$T.2, c(2, 3, 2, 4, 3, 3, 4, 5, 5, 2, 6, 7),
               label = format_vector_to_string(config$wrangled_data$T.2))

  expect_equal(nrow(config$wrangled_data), nrow(load$data),
               label = nrow(config$wrangled_data),
               expected.label = nrow(load$data))
  expect_equal(config$wrangled_data$Study, load$data$Study,
               label = format_vector_to_string(config$wrangled_data$Study),
               expected.label = format_vector_to_string(load$data$Study))
  expect_equal(config$wrangled_data$R.1, load$data$R.1,
               label = format_vector_to_string(config$wrangled_data$R.1),
               expected.label = format_vector_to_string(load$data$R.1))
  expect_equal(config$wrangled_data$N.1, load$data$N.1,
               label = format_vector_to_string(config$wrangled_data$N.1),
               expected.label = format_vector_to_string(load$data$N.1))
  expect_equal(config$wrangled_data$R.2, load$data$R.2,
               label = format_vector_to_string(config$wrangled_data$R.2),
               expected.label = format_vector_to_string(load$data$R.2))
  expect_equal(config$wrangled_data$N.2, load$data$N.2,
               label = format_vector_to_string(config$wrangled_data$N.2),
               expected.label = format_vector_to_string(load$data$N.2))
})


test_that("Covariate info is NULL when not available", {

  load <- setup_load(file.path(test_data_dir, "Cont_long.csv"), outcome = "Continuous")
  config <- setup_configure(load$data, load$treatment_df, "Continuous", "MD", "the Great")

  expect_null(config$covariate_column)
  expect_null(config$covariate_name)
  expect_null(config$covariate_type)

})

test_that("Covariate info is extracted when available", {

  load <- setup_load(file.path(test_data_dir, "Cont_long_continuous_cov.csv"), outcome = "Continuous")
  config <- setup_configure(load$data, load$treatment_df, "Continuous", "MD", "the Great")

  expect_equal(config$covariate_column, "covar.age")
  expect_equal(config$covariate_name, "age")
  expect_equal(config$covariate_type, "Continuous")

})
