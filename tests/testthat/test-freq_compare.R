test_that("freq_compare produces errors for incorrect data types and invalid values", {
  expect_error(freq_compare("not_a_list", "fixed", "good"), "freq must be of class list")
  expect_error(freq_compare(excluded_data_con$freq_sub, 123, "good"), "model_type must be of class character")
  expect_error(freq_compare(excluded_data_con$freq_sub, "fixed", 123), "ranking_option must be of class character")
  expect_error(freq_compare(excluded_data_con$freq_sub, "invalid_model_type", "good"), "model_type must be 'fixed' or 'random'")
})

test_that("freq_compare functions correctly", {
  result <- freq_compare(defined_data_con$freq_all, "fixed", "good")
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 4)

  result <- freq_compare(excluded_data_con$freq_sub, "fixed", "good")
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 3)

})

test_that("freq_compare produces downloadable tables", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study02", "Study03", "Study04"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(freqSel = "freq_compare")
  app$click("freq_compare-run")

  # don't know why, but the downloads fail without this
  common <- app$get_value(export = "common")

  table_all <- app$get_download("freq_compare-download_all")
  df <- read.csv(table_all)
  expect_equal(ncol(df), 5)
  expect_equal(nrow(df), 4)

  table_sub <- app$get_download("freq_compare-download_sub")
  df <- read.csv(table_sub)
  expect_equal(ncol(df), 4)
  expect_equal(nrow(df), 3)

  app$stop()

})
