test_that("summary_char produces errors for incorrect inputs", {
  expect_error(summary_char("not_data"), "configured_data must be of class configured_data")
})

test_that("summary_char functions correctly", {
  result <- summary_char(configured_data_con)

  expect_setequal(c("network", "treatments", "pairs"), colnames(result$network))

  expect_s3_class(result$network, "data.frame")
  expect_equal(ncol(result$network), 2)
  expect_equal(nrow(result$network), 9)
  expect_setequal(c("Value", "Characteristic"), colnames(result$network))

  result_sub <- summary_char(excluded_data_con)
  expect_equal(ncol(result_sub$network), 2)
  expect_equal(nrow(result_sub$network), 9)
  expect(!identical(result_sub$network$Value[3], result$network$Value[3]), "The subsetted summary is not different to the summary of all studies")

  result <- summary_char(configured_data_bin)
  expect_setequal(c("network", "treatments", "pairs"), colnames(result$network))
})

test_that("summary_char produces a merged table that can be downloaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  reload_app(app, config_path)
  app$set_inputs(tabs = "summary")
  app$set_inputs(summarySel = "summary_char")
  app$click("summary_char-run")

  app$wait_for_value(output = "summary_char-network")
  network <- app$get_value(output = "summary_char-network")
  expect_match(network, "<table")
  network_dl <- app$get_download("summary_char-download_network")
  df <- read.csv(network_dl)
  expect_equal(ncol(df), 3) # names are now a column
  expect_equal(nrow(df), 9)
  expect_true(all(c("All.studies", "With.selected.studies.excluded") %in% colnames(df)[2:3]))

  app$stop()
})

