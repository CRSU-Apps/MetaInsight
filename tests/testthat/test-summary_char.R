test_that("summary_char produces errors for incorrect inputs", {
  expect_error(summary_char("not_a_dataframe", "Continuous"), "bugsnet_data must be of class data.frame")
  expect_error(summary_char(defined_data_con$bugsnet_all, 123), "outcome must be of class character")
  expect_error(summary_char(defined_data_con$bugsnet_all, "invalid_outcome"), "outcome must be either Binary or Continuous")
})

test_that("summary_char functions correctly", {
  result <- summary_char(defined_data_con$bugsnet_all, "Continuous")
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 9)
  expect_true(all(c("Value", "Characteristic") %in% colnames(result)))

  result_sub <- summary_char(excluded_data_con$bugsnet_sub, "Continuous")
  expect_equal(ncol(result_sub), 2)
  expect_equal(nrow(result_sub), 9)
  expect(!identical(result_sub$Value[3], result$Value[3]), "The subsetted summary is not different to the summary of all studies")
})

test_that("summary_char produces a merged table that can be downloaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$set_inputs(summarySel = "summary_char")
  app$set_inputs(main = "Results")
  result_table <- app$get_download("summary_char-download")
  df <- read.csv(result_table)
  expect_equal(ncol(df), 3) # names are now a column
  expect_equal(nrow(df), 9)
  expect_true(all(c("All.studies", "With.selected.studies.excluded") %in% colnames(df)[2:3]))
})

