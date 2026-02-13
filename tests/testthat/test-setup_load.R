data_path <- system.file("extdata", "continuous_long.csv", package = "metainsight")
invalid_data_path <- file.path(test_data_dir, "invalid_data", "binary-missing-long.csv")

test_that("Check setup_load function returns errors for faulty inputs", {
  expect_error(setup_load(data_path = 123, outcome = "binary"), "data_path must be of class character")
  expect_error(setup_load(data_path = "word_doc.docx", outcome = "binary"), "data_path must link to either a .csv or .xlsx file")
  expect_error(setup_load(data_path = "non_existent.csv", outcome = "binary"), "The specified file does not exist")
  expect_error(setup_load(outcome = 123), "outcome must be of class character")
  expect_error(setup_load(outcome = "not_binary"), "outcome must be either binary or continuous")
})

test_that("Check setup_load function loads default data when no path is specified", {
  expected_components <- c("is_data_valid", "is_data_uploaded", "data", "treatments", "outcome")
  result <- setup_load(outcome = "binary")
  expect_true(all(expected_components %in% names(result)))
  result <- setup_load(outcome = "continuous")
  expect_true(all(expected_components %in% names(result)))
})

test_that("Check setup_load returns the correct data classes", {
  result <- setup_load(outcome = "continuous")
  expect_type(result$is_data_valid, "logical")
  expect_type(result$is_data_uploaded, "logical")
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$treatments, "data.frame")
})

# only using one example here as all the others are tested in test-data_validation.R
test_that("Check setup_load handles invalid data correctly", {
  expect_error(setup_load(data_path = invalid_data_path, outcome = "binary"), "*Missing columns for binary data: N*")
})

# from here, tests have been refactored from test-data_input_panel.R
test_that("continuous wide data matches between .csv and .xlsx files", {
  result_csv <- setup_load(data_path = file.path("data", "Non_opioids_wide.csv"), outcome = "continuous")
  result_xlsx <- setup_load(data_path = file.path("data", "Non_opioids_wide.xlsx"), outcome = "continuous")
  expect_equal(result_csv, result_xlsx)
})

test_that("continuous long data matches between .csv and .xlsx files", {
  result_csv <- setup_load(data_path = file.path("data", "Non_opioids_long.csv"), outcome = "continuous")
  result_xlsx <- setup_load(data_path = file.path("data", "Non_opioids_long.xlsx"), outcome = "continuous")
  expect_equal(result_csv, result_xlsx)
})

test_that("continuous long data matches between .csv and .xlsx files", {
  result_csv <- setup_load(data_path = file.path("data", "Certolizumab_long.csv"), outcome = "binary")
  result_xlsx <- setup_load(data_path = file.path("data", "Certolizumab_long.xlsx"), outcome = "binary")
  expect_equal(result_csv, result_xlsx)
})

test_that("continuous long data matches between .csv and .xlsx files", {
  result_csv <- setup_load(data_path = file.path("data", "Certolizumab_wide.csv"), outcome = "binary")
  result_xlsx <- setup_load(data_path = file.path("data", "Certolizumab_wide.xlsx"), outcome = "binary")
  expect_equal(result_csv, result_xlsx)
})

test_that("Treatments and data extracted from default binary file", {
  result <- setup_load(outcome = "binary")
  treatments <- data.frame(Number = seq(7), Label = c('Placebo','Infliximab','Adalimumab','Tocilizumab','CZP','Rituximab', 'Etanercept'))
  expect_equal(result$treatments, treatments)

  expect_equal(colnames(result$data), c('Study', 'T', 'N', 'R', 'covar.duration'),
               label = format_vector_to_string(colnames(result$data)))
  expect_equal(nrow(result$data), 24,
               label = nrow(result$data))
  expect_equal(result$data$Study, rep(c('Abe2006', 'ARMADA', 'ATTEST', 'CHARISMA', 'DE019', 'Kim2007', 'OPTION', 'RAPID1', 'RAPID2', 'START', 'Strand2006', 'Weinblatt1999'), each = 2),
               label = format_vector_to_string(result$data$Study))
  expect_equal(result$data$T, c('Placebo', 'Infliximab', 'Placebo', 'Adalimumab', 'Placebo', 'Infliximab', 'Placebo', 'Tocilizumab', 'Placebo', 'Adalimumab', 'Placebo', 'Adalimumab', 'Placebo', 'Tocilizumab', 'Placebo', 'CZP', 'Placebo', 'CZP', 'Placebo', 'Infliximab', 'Placebo', 'Rituximab', 'Placebo', 'Etanercept'
  ),
  label = format_vector_to_string(result$data$T))
  expect_equal(result$data$R, c(0, 15, 5, 37, 22, 61, 14, 26, 19, 81, 9, 28, 22, 90, 15, 146, 4, 80, 33, 110, 5, 5, 1, 23),
               label = format_vector_to_string(result$data$R))
  expect_equal(result$data$N, c(47, 49, 62, 67, 110, 165, 49, 50, 200, 207, 63, 65, 204, 205, 199, 393, 127, 246, 363, 360, 40, 40, 30, 59),
               label = format_vector_to_string(result$data$N))

  expect_equal(result$outcome, "binary")

})

test_that("Check setup_load loads data into common correctly for default data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  common <- app$get_value(export = "common")

  expect_type(common$loaded_data$is_data_valid, "logical")
  expect_type(common$loaded_data$is_data_uploaded, "logical")
  expect_s3_class(common$loaded_data$data, "data.frame")
  expect_s3_class(common$loaded_data$treatments, "data.frame")

  expect_true(common$loaded_data$is_data_valid)
  expect_false(common$loaded_data$is_data_uploaded)

  expect_equal(colnames(common$loaded_data$data), c("Study", "T", "Mean", "SD", "N", "covar.age", "rob", "indirectness",
                                        "rob.randomisation", "rob.allocation_concealment", "rob.blinding_(participants_and_personnel)",
                                        "rob.blinding_(outcomes)", "rob.attrition", "rob.reporting",
                                        "rob.other_bias"),
               label = format_vector_to_string(colnames(common$loaded_data$data)))
  expect_equal(nrow(common$loaded_data$data), 90,
               label = nrow(common$loaded_data$data))
  expect_equal(common$loaded_data$data$Study, paste0("Study", stringr::str_pad(string = rep(1:45, each = 2), width = 2, pad = "0")),
               label = format_vector_to_string(common$loaded_data$data$Study))
  expect_equal(common$loaded_data$data$T, c(rep(c("Placebo", "Glucocorticoids"), times = 4),
                           rep(c("Placebo", "Ketamine"), times = 18),
                           rep(c("Placebo", "Gabapentinoids"), times = 23)
  ),
  label = format_vector_to_string(common$loaded_data$data$T))
  expect_equal(common$loaded_data$data$N, c(50, 62, 45, 92, 88, 78, 47, 46, 52, 43, 84, 84, 34, 33, 35, 34, 66, 73, 19, 18, 6, 8, 47, 31, 24, 25, 17, 15, 35, 33, 42, 49, 62, 118, 22, 22, 7, 8, 25, 55, 38, 39, 30, 31, 10, 21, 53, 65, 20, 20, 35, 27, 22, 23, 4, 2, 30, 60, 50, 50, 20, 20, 29, 30,
                           24, 17, 49, 50, 28, 83, 100, 100, 24, 22, 48, 52, 31, 62, 20, 18, 18, 16, 46, 46, 27, 27, 35, 70, 76, 75),
               label = format_vector_to_string(common$loaded_data$data$N))
  expect_equal(common$loaded_data$data$Mean, c(2.2, 1.9, 2, 2, 1.4, 1.4, 2.9, 2.6, 5.4, 3, 1, 1, 0, 0, 0, 0, 4, 4, 0.3, 1.1, 0.5, 2, 1.3, 1.4, 0, 0, 1.5, .3, 1, 0.7, 4, 4.3, 2, 2, 1.5, 0, 3.4, 2.4, 1, 0.5, 0.1, 0.2, 0.8, 0.8, 3.4, 3.1, 0, 0, 0.6, 0.2, 0.6, 0.5, 2.2, 2.6, 2.5, 3, 1.2, 0.5, 2.2,2.1, 0.2, 0.1, 2, 1, 1.1, 0.9, 3, 2, 2, 1.8, 3.4, 2.5, 1.9, 1.5, 0, 0, 3, 2, 1.5, 1.1, 0.5, 1, 0, 0, 1.5, 1.3, 0.03, 0.4, 1.2, 1.4),
               label = format_vector_to_string(common$loaded_data$data$Mean))
  expect_equal(common$loaded_data$data$SD, c(2.5, 2.4, 3.7, 2.8, 1.6, 1.7, 2.5, 2.4, 3.6, 3.5, 1.5, 1.5, 0.2, 0.7, 0.1, 0.3, 3.3, 2.2, 0.7, 2.1, 3.7, 1.7, 1.5, 1.4, 1.9, 0.7, 4.4, 3, 1.7, 1.4, 2.1, 2.5, 1.5, 1.5, 2.2, 1.5, 3, 1.1, 1.8, 1.7, 0.7, 0.7, 1.1, 0.9, 1.6, 1.7, 0.7, 0.7, 0.9, 0.4, 0.1,0.9, 1.4, 1.7, 1, 0.8, 1.5, 0.9, 0.9, 1.2, 0.1, 0.1, 1.5, 0.6, 1.5, 1.2, 1.9, 1.5, 2.5, 2, 1.4, 1.2, 2, 1.9, 0.7, 2.1, 0.8, 0.5, 2, 1.6, 1.5, 1.5, 0.7, 0.7, 2.8, 2.2, 0.2, 0.9, 1.7, 1.7),
               label = format_vector_to_string(common$loaded_data$data$SD))

  expect_equal(common$loaded_data$treatments, data.frame(Number = seq(4), Label = c('Placebo','Glucocorticoids','Ketamine','Gabapentinoids')))

})

test_that("Check setup_load loads data into common correctly for an uploaded file", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$upload_file("setup_load-file1" = data_path)
  app$click("setup_load-run")
  common <- app$get_value(export = "common")

  expect_type(common$loaded_data$is_data_valid, "logical")
  expect_type(common$loaded_data$is_data_uploaded, "logical")
  expect_s3_class(common$loaded_data$data, "data.frame")
  expect_s3_class(common$loaded_data$treatments, "data.frame")

  expect_true(common$loaded_data$is_data_valid)
  expect_true(common$loaded_data$is_data_uploaded)
})

test_that("Invalid data is loaded into common correctly and errors are passed to the logger", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$set_inputs("setup_load-outcome" = "binary")
  app$upload_file("setup_load-file1" = invalid_data_path)
  app$click("setup_load-run")
  common <- app$get_value(export = "common")

  expect_type(common$loaded_data$is_data_valid, "logical")
  expect_type(common$loaded_data$is_data_uploaded, "logical")
  expect_s3_class(common$loaded_data$data, "data.frame")
  expect_null(common$loaded_data$treatments, "data.frame")
  expect_false(common$loaded_data$is_data_valid)
  expect_true(common$loaded_data$is_data_uploaded)

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*Missing columns for binary data*", logger))
})


test_that("Data can be reloaded after loading", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  common <- app$get_value(export = "common")
  expect_s3_class(common$loaded_data$data, "data.frame")
  expect_equal(common$loaded_data$outcome, "continuous")

  # when only setup_load is run, new data should be uploadable without deleting
  app$set_inputs("setup_load-outcome" = "binary")
  app$click("setup_load-run")
  common <- app$get_value(export = "common")
  expect_s3_class(common$loaded_data$data, "data.frame")
  expect_equal(common$loaded_data$outcome, "binary")

  # reset data
  app$click("setup_load-reset")
  # click the confirm button
  app$wait_for_js("$('.sweet-alert.visible').length > 0")
  app$click(selector = ".confirm")
  app$wait_for_idle()
  common <- app$get_value(export = "common")
  expect_null(common$loaded_data)
})

test_that("Data can be reloaded after loading", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  # load data again and configure
  app$set_inputs("setup_load-outcome" = "continuous")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  common <- app$get_value(export = "common")
  # app$view()
  expect_s3_class(common$configured_data, "configured_data")

  # now loading again should generate an error
  app$click("setup_load-run")
  logger <- app$get_value(export = "logger")
  expect_true(grepl("*Data has already been loaded*", logger))

  # reset data
  app$click("setup_load-reset")
  # click the confirm button
  app$wait_for_js("$('.sweet-alert.visible').length > 0")
  app$click(selector = ".confirm")
  app$wait_for_idle()
  common <- app$get_value(export = "common")
  expect_null(common$loaded_data)

})


