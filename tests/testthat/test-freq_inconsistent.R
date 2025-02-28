test_that("freq_inconsistent produces errors for incorrect data types and invalid values", {
  expect_error(freq_inconsistent("not_a_list", "fixed"), "freq must be of class list")
  expect_error(freq_inconsistent(excluded_data_con$freq_sub, 123), "model_type must be of class character")
  expect_error(freq_inconsistent(excluded_data_con$freq_sub, "invalid_model_type"), "model_type must be 'fixed' or 'random'")
})

test_that("freq_compare functions correctly", {
  result <- freq_inconsistent(defined_data_con$freq_all, "fixed")
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 6)

  result <- freq_inconsistent(excluded_data_con$freq_sub, "fixed")
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 3)

})

test_that("freq_inconsistent produces downloadable tables", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study02", "Study03", "Study04"))
  app$wait_for_value(input = "summary_exclude-complete")
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_inconsistent")
  app$click("freq_inconsistent-run")

  common <- app$get_value(export = "common")

  table_all <- app$get_download("freq_inconsistent-download_all")
  df <- read.csv(table_all)
  df <- df[, -1] # drop index

  expect_equal(ncol(df), 9)
  expect_equal(nrow(df), 6)

  table_sub <- app$get_download("freq_inconsistent-download_sub")
  df <- read.csv(table_sub)
  df <- df[, -1] # drop index

  expect_equal(ncol(df), 9)
  expect_equal(nrow(df), 3)

})
