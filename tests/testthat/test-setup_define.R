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

# this and the next are equivalent to the 3rd and 6th in test-load_data_page.R

test_that("setup_define loads data into common correctly for continuous long data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = "data/Cont_long_continuous_cov.csv")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$initial_non_covariate_data, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment, "the Great")
  expect_equal(common$outcome_measure, "MD")

  app$stop()
})

test_that("setup_define loads data into common correctly for wide binary data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = system.file("extdata", "binary_wide.csv", package = "metainsight"))
  app$set_inputs("setup_load-outcome" = "Binary")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
  expect_s3_class(common$treatment_df, "data.frame")
  expect_length(common$disconnected_indices, 0)
  expect_s3_class(common$main_connected_data, "data.frame")
  expect_s3_class(common$initial_non_covariate_data, "data.frame")
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_type(common$freq_all, "list")
  expect_equal(common$reference_treatment, "Placebo")
  expect_equal(common$outcome_measure, "OR")

  app$stop()
})

test_that("setup_define logs errors when disconnected data is uploaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = "data/continuous_long_disconnected.csv")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_s3_class(common$wrangled_data, "data.frame")
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

  app$stop()
})

# From here on, refactored from test-load_data_page.R
# These could be refactored further as there's no need to be E2E
test_that("Data wrangled from default continuous long file", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_equal(colnames(common$wrangled_data), c("StudyID", colnames(common$data)[c(1, 2, 5, 3, 4, 6)]),
               label = format_vector_to_string(colnames(common$wrangled_data)))

  expect_equal(common$wrangled_data$StudyID, rep(1:45, each = 2),
               label = format_vector_to_string(common$wrangled_data$StudyID))
  expect_equal(common$wrangled_data$T, c(rep(c(1, 2), times = 4), rep(c(1, 3), times = 18), rep(c(1, 4), times = 23)),
               label = format_vector_to_string(common$wrangled_data$T))

  expect_equal(nrow(common$wrangled_data), nrow(common$data),
               label = nrow(common$wrangled_data),
               expected.label = nrow(common$data))
  expect_equal(common$wrangled_data$Study, common$data$Study,
               label = format_vector_to_string(common$wrangled_data$Study),
               expected.label = format_vector_to_string(common$data$Study))
  expect_equal(common$wrangled_data$N, common$data$N,
               label = format_vector_to_string(common$wrangled_data$N),
               expected.label = format_vector_to_string(common$data$N))
  expect_equal(common$wrangled_data$Mean, common$data$Mean,
               label = format_vector_to_string(common$wrangled_data$Mean),
               expected.label = format_vector_to_string(common$data$Mean))
  expect_equal(common$wrangled_data$SD, common$data$SD,
               label = format_vector_to_string(common$wrangled_data$SD),
               expected.label = format_vector_to_string(common$data$SD))
  app$stop()

})

test_that("Continuous wide data wrangled with treatment IDs", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-data" = system.file("extdata", "continuous_wide.csv", package = "metainsight"))
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_equal(common$wrangled_data$StudyID, 1:45,
               label = format_vector_to_string(common$wrangled_data$StudyID))
  expect_equal(common$wrangled_data$T.1, c(rep(1, times = 45)),
               label = format_vector_to_string(common$wrangled_data$T.1))
  expect_equal(common$wrangled_data$T.2, c(rep(2, times = 4), rep(3, times = 18), rep(4, times = 23)),
               label = format_vector_to_string(common$wrangled_data$T.2))

  expect_equal(nrow(common$wrangled_data), nrow(common$data),
               label = nrow(common$wrangled_data),
               expected.label = nrow(common$data))
  expect_equal(common$wrangled_data$Study, common$data$Study,
               label = format_vector_to_string(common$wrangled_data$Study),
               expected.label = format_vector_to_string(common$data$Study))
  expect_equal(common$wrangled_data$N.1, common$data$N.1,
               label = format_vector_to_string(common$wrangled_data$N.1),
               expected.label = format_vector_to_string(common$data$N.1))
  expect_equal(common$wrangled_data$Mean.1, common$data$Mean.1,
               label = format_vector_to_string(common$wrangled_data$Mean.1),
               expected.label = format_vector_to_string(common$data$Mean.1))
  expect_equal(common$wrangled_data$SD.1, common$data$SD.1,
               label = format_vector_to_string(common$wrangled_data$SD.1),
               expected.label = format_vector_to_string(common$data$SD.1))
  expect_equal(common$wrangled_data$N.2, common$data$N.2,
               label = format_vector_to_string(common$wrangled_data$N.2),
               expected.label = format_vector_to_string(common$data$N.2))
  expect_equal(common$wrangled_data$Mean.2, common$data$Mean.2,
               label = format_vector_to_string(common$wrangled_data$Mean.2),
               expected.label = format_vector_to_string(common$data$Mean.2))
  expect_equal(common$wrangled_data$SD.2, common$data$SD.2,
               label = format_vector_to_string(common$wrangled_data$SD.2),
               expected.label = format_vector_to_string(common$data$SD.2))
  app$stop()

})


test_that("Data wrangled from default binary long file", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$set_inputs("setup_load-outcome" = "Binary")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_equal(colnames(common$wrangled_data), c("StudyID", colnames(common$data)[c(1, 2, 4, 3, 5)]),
               label = format_vector_to_string(colnames(common$wrangled_data)))

  expect_equal(common$wrangled_data$StudyID, rep(1:12, each = 2),
               label = format_vector_to_string(common$wrangled_data$StudyID))
  expect_equal(common$wrangled_data$T, c(1, 2, 1, 3, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, 1, 5, 1, 5, 1, 2, 1, 6, 1, 7),
               label = format_vector_to_string(common$wrangled_data$T))

  expect_equal(nrow(common$wrangled_data), nrow(common$data),
               label = nrow(common$wrangled_data),
               expected.label = nrow(common$data))
  expect_equal(common$wrangled_data$Study, common$data$Study,
               label = format_vector_to_string(common$wrangled_data$Study),
               expected.label = format_vector_to_string(common$data$Study))
  expect_equal(common$wrangled_data$R, common$data$R,
               label = format_vector_to_string(common$wrangled_data$R),
               expected.label = format_vector_to_string(common$data$R))
  expect_equal(common$wrangled_data$N, common$data$N,
               label = format_vector_to_string(common$wrangled_data$N),
               expected.label = format_vector_to_string(common$data$N))
  app$stop()
})

test_that("Binary wide data wrangled with treatment IDs", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$set_inputs("setup_load-outcome" = "Binary")
  app$upload_file("setup_load-data" = system.file("extdata", "binary_wide.csv", package = "metainsight"))
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  common <- app$get_value(export = "common")

  expect_equal(common$wrangled_data$StudyID, 1:12,
               label = format_vector_to_string(common$wrangled_data$StudyID))
  expect_equal(common$wrangled_data$T.1, rep(1, times = 12),
               label = format_vector_to_string(common$wrangled_data$T.1))
  expect_equal(common$wrangled_data$T.2, c(2, 3, 2, 4, 3, 3, 4, 5, 5, 2, 6, 7),
               label = format_vector_to_string(common$wrangled_data$T.2))

  expect_equal(nrow(common$wrangled_data), nrow(common$data),
               label = nrow(common$wrangled_data),
               expected.label = nrow(common$data))
  expect_equal(common$wrangled_data$Study, common$data$Study,
               label = format_vector_to_string(common$wrangled_data$Study),
               expected.label = format_vector_to_string(common$data$Study))
  expect_equal(common$wrangled_data$R.1, common$data$R.1,
               label = format_vector_to_string(common$wrangled_data$R.1),
               expected.label = format_vector_to_string(common$data$R.1))
  expect_equal(common$wrangled_data$N.1, common$data$N.1,
               label = format_vector_to_string(common$wrangled_data$N.1),
               expected.label = format_vector_to_string(common$data$N.1))
  expect_equal(common$wrangled_data$R.2, common$data$R.2,
               label = format_vector_to_string(common$wrangled_data$R.2),
               expected.label = format_vector_to_string(common$data$R.2))
  expect_equal(common$wrangled_data$N.2, common$data$N.2,
               label = format_vector_to_string(common$wrangled_data$N.2),
               expected.label = format_vector_to_string(common$data$N.2))
  app$stop()
})
