test_that("freq_compare produces errors for incorrect data types and invalid values", {
  expect_error(freq_compare("not_data"), "configured_data must be of class configured_data")
})

test_that("freq_compare functions correctly", {
  n_trt_all <- configured_data_con$freq$ntx
  n_trt_sub <- excluded_data_con$freq$ntx

  result <- freq_compare(configured_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), n_trt_all)
  expect_equal(nrow(result), n_trt_all)

  result <- freq_compare(excluded_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), n_trt_sub)
  expect_equal(nrow(result), n_trt_sub)
})

test_that("freq_compare produces downloadable tables", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  reload_app(app, config_path)

  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_compare")
  app$click("freq_compare-run")

  app$wait_for_value(output = "freq_compare-table_all")
  app$wait_for_value(output = "freq_compare-table_sub")

  table_all <- app$get_value(output = "freq_compare-table_all")
  table_sub <- app$get_value(output = "freq_compare-table_sub")

  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")
  expect_false(identical(table_all, table_sub))

  table_all <- app$get_download("freq_compare-download_all")
  df <- read.csv(table_all)
  expect_equal(ncol(df), n_trt_all + 1)
  expect_equal(nrow(df), n_trt_all)

  table_sub <- app$get_download("freq_compare-download_sub")
  df <- read.csv(table_sub)
  expect_equal(ncol(df), n_trt_sub + 1)
  expect_equal(nrow(df), n_trt_sub)

  app$stop()

})
