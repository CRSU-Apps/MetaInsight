mock_data <- setup_load(file.path(test_data_dir,"Cont_long_continuous_cov.csv"), "continuous")
mock_data_bin <- setup_load(file.path(test_data_dir, "Binary_long_binary_cov.csv"), "binary")

test_that("setup_configure returns errors for faulty inputs", {
  expect_error(setup_configure("not_loaded", "the Great", "fixed", "MD", "good", 999), "loaded_data must be of class loaded_data")
  expect_error(setup_configure(mock_data, 123, "fixed", "MD", "good", 999), "reference_treatment must be of class character")
  expect_error(setup_configure(mock_data, "the Great", 123, "MD", "good", 999), "effects must be of class character")
  expect_error(setup_configure(mock_data, "the Great", "fixed", 123, "good", 999), "outcome_measure must be of class character")
  expect_error(setup_configure(mock_data, "the Great", "fixed", "MD", 123, 999), "ranking_option must be of class character")
  expect_error(setup_configure(mock_data, "the Great", "fixed", "MD", "good", "999"), "seed must be of class numeric")

  expect_error(setup_configure(mock_data, "invalid", "fixed", "MD", "good", 999), "reference_treatment must be present in the loaded data")
  expect_error(setup_configure(mock_data, "the Great", "fixed", "InvalidMeasure", "good", 999), "outcome_measure must be either MD or SMD")
  expect_error(setup_configure(mock_data_bin, "the Great", "fixed", "InvalidMeasure", "good", 999), "outcome_measure must be either OR, RR or RD")
  expect_error(setup_configure(mock_data, "the Great", "fixed", "MD", "not_good", 999), "ranking_option must be either good or bad")

})

test_that("setup_configure returns correctly structured objects", {

  expected_items <- c("wrangled_data", "treatments", "disconnected_indices", "connected_data",
    "non_covariate_data", "covariate", "bugsnet", "freq", "outcome", "outcome_measure", "effects", "ranking_option", "seed")

  result <- setup_configure(mock_data, "the Great", "fixed", "MD", "good", 999)
  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))

  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_length(result$disconnected_indices, 0)
  expect_s3_class(result$connected_data, "data.frame")
  expect_s3_class(result$non_covariate_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_type(result$covariate$column, "character")
  expect_type(result$covariate$name, "character")
  expect_type(result$covariate$type, "character")
  expect_s3_class(result$bugsnet, "data.frame")
  expect_type(result$freq, "list")
  expect_type(result$outcome, "character")
  expect_type(result$outcome_measure, "character")
  expect_type(result$effects, "character")
  expect_type(result$ranking_option, "character")
  expect_type(result$seed, "double")
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

  result <- common$configured_data
  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_length(result$disconnected_indices, 0)
  expect_s3_class(result$connected_data, "data.frame")
  expect_s3_class(result$non_covariate_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_type(result$covariate$column, "character")
  expect_type(result$covariate$name, "character")
  expect_type(result$covariate$type, "character")
  expect_s3_class(result$bugsnet, "data.frame")
  expect_type(result$freq, "list")
  expect_type(result$outcome, "character")
  expect_type(result$outcome_measure, "character")
  expect_type(result$effects, "character")
  expect_type(result$ranking_option, "character")
  expect_type(result$seed, "double")

  expect_equal(result$reference_treatment, "the_Great")
  expect_equal(result$outcome, "continuous")
  expect_equal(result$outcome_measure, "MD")

  app$stop()
})

test_that("setup_configure loads data into common correctly for wide binary data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = system.file("extdata", "binary_wide.csv", package = "metainsight"))
  app$set_inputs("setup_load-outcome" = "binary")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")

  result <- common$configured_data
  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_length(result$disconnected_indices, 0)
  expect_s3_class(result$connected_data, "data.frame")
  expect_s3_class(result$non_covariate_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_type(result$covariate$column, "character")
  expect_type(result$covariate$name, "character")
  expect_type(result$covariate$type, "character")
  expect_s3_class(result$bugsnet, "data.frame")
  expect_type(result$freq, "list")
  expect_type(result$outcome, "character")
  expect_type(result$outcome_measure, "character")
  expect_type(result$effects, "character")
  expect_type(result$ranking_option, "character")
  expect_type(result$seed, "double")

  expect_equal(result$reference_treatment, "Placebo")
  expect_equal(result$outcome, "binary")
  expect_equal(result$outcome_measure, "OR")

  app$stop()
})

test_that("setup_configure logs errors when disconnected data is uploaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "continuous_long_disconnected.csv"))
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")

  result <- common$configured_data
  expect_s3_class(result$wrangled_data, "data.frame")
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_length(result$disconnected_indices, 3)
  expect_s3_class(result$connected_data, "data.frame")
  expect_s3_class(result$non_covariate_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_s3_class(result$bugsnet, "data.frame")
  expect_type(result$freq, "list")
  expect_equal(result$reference_treatment, "A")
  expect_equal(result$outcome_measure, "MD")
  expect_equal(result$outcome, "continuous")

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*The uploaded data comprises a disconnected network*", logger))

  app$stop()
})

test_that("Data wrangled from default continuous long file", {

  load <- setup_load(outcome = "continuous")
  config <- setup_configure(load, "Placebo", "random", "MD", "good", 999)

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

  load <- setup_load(file.path(test_data_dir, "Non_opioids_wide.csv"), outcome = "continuous")
  config <- setup_configure(load, "Placebo", "random", "MD", "good", 999)

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

  load <- setup_load(outcome = "binary")
  config <- setup_configure(load, "Placebo", "random", "OR", "good", 999)

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

  load <- setup_load(file.path(test_data_dir, "Certolizumab_wide.csv"), outcome = "binary")
  config <- setup_configure(load, "Placebo", "random", "OR", "good", 999)

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

  load <- setup_load(file.path(test_data_dir, "Cont_long.csv"), outcome = "continuous")
  config <-setup_configure(load, "the Great", "random", "MD", "good", 999)

  expect_length(config$covariate, 0)

})

test_that("Covariate info is extracted when available", {

  load <- setup_load(file.path(test_data_dir, "Cont_long_continuous_cov.csv"), outcome = "continuous")
  config <-setup_configure(load, "the Great", "random", "MD", "good", 999)

  expect_equal(config$covariate$column, "covar.age")
  expect_equal(config$covariate$name, "age")
  expect_equal(config$covariate$type, "continuous")

})
