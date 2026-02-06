test_that("freq_compare functions correctly", {
  result <- freq_inconsistent(configured_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 6)

  result <- freq_inconsistent(excluded_data_con)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 3)

  result <- freq_inconsistent(configured_data_bin)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 21)

})

test_that("freq_inconsistent produces errors for incorrect data types and invalid values", {
  expect_error(freq_inconsistent("not_data"), "configured_data must be of class configured_data")
})

test_that("freq_inconsistent produces downloadable tables", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$wait_for_value(input = "setup_configure-ready")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$wait_for_idle()
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study02", "Study03", "Study04"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_inconsistent")
  app$click("freq_inconsistent-run")

  app$wait_for_value(output = "freq_inconsistent-table_all")
  app$wait_for_value(output = "freq_inconsistent-table_sub")

  table_all <- app$get_value(output = "freq_inconsistent-table_all")
  table_sub <- app$get_value(output = "freq_inconsistent-table_sub")

  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")

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

  app$stop()
})
