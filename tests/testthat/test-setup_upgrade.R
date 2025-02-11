old_data_path <- system.file("extdata", "old_data.csv", package = "metainsight")

test_that("setup_upgrade returns errors for faulty inputs", {
  expect_error(setup_upgrade(123, "A,B,C,D,E"), "data_path must be of class character")
  expect_error(setup_upgrade("word_doc.docx", "A,B,C,D,E"), "data_path must link to either a .csv file")
  expect_error(setup_upgrade("non_existent.csv", "A,B,C,D,E"), "The specified file does not exist")
  expect_error(setup_upgrade(old_data_path, 123), "treatments must be of class character")
  expect_error(setup_upgrade(old_data_path, "123"), "The treatment names must only contain words separated by commas")
  expect_error(setup_upgrade(old_data_path, "A,B"), "Your input data contains 2 treatments")
})

# think this is sufficient for now as there a lot of other tests in test-data_upgrade.R
test_that("setup_upgrade functions correctly", {
  result <- setup_upgrade(old_data_path, "A,B,C,D,E")
  expect_s3_class(result, "data.frame")
})

test_that("setup_upgrade loads data into common and can be downloaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_upgrade")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_upgrade")
  app$upload_file("setup_upgrade-uploaded_data" = old_data_path)
  app$set_inputs("setup_upgrade-treatment_names" = "A,B,C,D,E")
  app$click("setup_upgrade-run")
  common <- app$get_value(export = "common")
  expect_s3_class(common$upgraded_data, "data.frame")
  upgraded <- app$get_download("setup_upgrade-download")
  df <- read.csv(upgraded)
  expect_equal(nrow(df), 7)
})

test_that("setup_upgrade returns errors", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_upgrade")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_upgrade")
  app$upload_file("setup_upgrade-uploaded_data" = old_data_path)
  app$set_inputs("setup_upgrade-treatment_names" = "A,B")
  app$click("setup_upgrade-run")
  common <- app$get_value(export = "common")
  expect_null(common$upgraded_data)

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*Your input data contains 2 treatments*", logger))
})

