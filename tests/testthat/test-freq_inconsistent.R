test_that("freq_compare functions correctly", {

  result <- freq_inconsistency(configured_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), sum(1:(n_trt_all-1)))

  result <- freq_inconsistency(excluded_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), sum(1:(n_trt_sub-1)))

  result <- freq_inconsistency(configured_data_bin)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), sum(1:(n_trt_bin-1)))

})

test_that("freq_inconsistency produces errors for incorrect data types and invalid values", {
  expect_error(freq_inconsistency("not_data"), "configured_data must be of class configured_data")
})

test_that("freq_inconsistency produces downloadable tables", {
  skip_if(skip_shinytest2)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  reload_app(app, config_path)

  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_inconsistency")
  app$click("freq_inconsistency-run")

  app$wait_for_value(output = "freq_inconsistency-table_all")
  app$wait_for_value(output = "freq_inconsistency-table_sub")

  table_all <- app$get_value(output = "freq_inconsistency-table_all")
  table_sub <- app$get_value(output = "freq_inconsistency-table_sub")

  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")

  table_all <- app$get_download("freq_inconsistency-download_all")
  df <- read.csv(table_all)
  df <- df[, -1] # drop index

  expect_equal(ncol(df), 9)
  expect_equal(nrow(df), sum(1:(n_trt_all-1)))

  table_sub <- app$get_download("freq_inconsistency-download_sub")
  df <- read.csv(table_sub)
  df <- df[, -1] # drop index

  expect_equal(ncol(df), 9)
  expect_equal(nrow(df), sum(1:(n_trt_sub-1)))

  app$stop()
})
