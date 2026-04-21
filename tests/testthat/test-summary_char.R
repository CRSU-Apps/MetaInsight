test_that("summary_char produces errors for incorrect inputs", {
  expect_error(summary_char("not_data"), "configured_data must be of class configured_data")
})

test_that("summary_char functions correctly", {
  result <- summary_char(configured_data_con)
  expect_setequal(c("network", "treatments", "pairs"), names(result))

  expect_s3_class(result$network, "data.frame")
  expect_equal(ncol(result$network), 2)
  expect_equal(nrow(result$network), 9)
  expect_setequal(c("Value", "Characteristic"), colnames(result$network))

  expect_equal(ncol(result$treatments), 6)
  expect_equal(nrow(result$treatments), n_trt_all)
  expect_setequal(c("Treatment", "Number of studies", "Number of patients",
                    "Minimum outcome", "Average outcome", "Maximum outcome"), colnames(result$treatments))

  expect_equal(ncol(result$pairs), 3)
  expect_equal(nrow(result$pairs), n_comparisons_all)
  expect_setequal(c("Treatment comparison", "Number of studies", "Number of patients"), colnames(result$pairs))


  result_sub <- summary_char(excluded_data_con)
  expect_setequal(c("network", "treatments", "pairs"), names(result_sub))

  expect_equal(ncol(result_sub$network), 2)
  expect_equal(nrow(result_sub$network), 9)
  expect_setequal(c("Value", "Characteristic"), colnames(result_sub$network))
  expect(!identical(result_sub$network$Value[3], result$network$Value[3]), "The subsetted summary is not different to the summary of all studies")

  expect_equal(ncol(result_sub$treatments), 6)
  expect_equal(nrow(result_sub$treatments), n_trt_sub)
  expect_setequal(c("Treatment", "Number of studies", "Number of patients",
                    "Minimum outcome", "Average outcome", "Maximum outcome"), colnames(result_sub$treatments))

  expect_equal(ncol(result_sub$pairs), 3)
  expect_equal(nrow(result_sub$pairs), n_comparisons_sub)
  expect_setequal(c("Treatment comparison", "Number of studies", "Number of patients"), colnames(result_sub$pairs))


  result_bin <- summary_char(configured_data_bin)
  expect_setequal(c("network", "treatments", "pairs"), names(result_bin))
  expect_equal(ncol(result_bin$network), 2)
  expect_equal(nrow(result_bin$network), 12)
  expect_setequal(c("Value", "Characteristic"), colnames(result_bin$network))

  expect_equal(ncol(result_bin$treatments), 7)
  expect_equal(nrow(result_bin$treatments), n_trt_bin)
  expect_setequal(c("Treatment", "Number of studies", "Number of events", "Number of patients",
                    "Minimum outcome", "Average outcome", "Maximum outcome"), colnames(result_bin$treatments))

  expect_equal(ncol(result_bin$pairs), 5)
  expect_equal(nrow(result_bin$pairs), n_comparisons_bin)
  expect_setequal(c("Treatment comparison", "Number of studies", "Number of patients",
                    "Number of outcomes", "Proportion"), colnames(result_bin$pairs))

})

test_that("summary_char produces tables that can be downloaded", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  reload_app(app, config_path)
  app$set_inputs(tabs = "summary")
  app$set_inputs(summarySel = "summary_char")
  app$click("summary_char-run")

  network <- app$wait_for_value(output = "summary_char-network")
  expect_match(network, "<table")

  trt_all <- app$wait_for_value(output = "summary_char-treatments_all")
  trt_sub <- app$wait_for_value(output = "summary_char-treatments_sub")
  expect_match(trt_all, "<table")
  expect_match(trt_sub, "<table")

  pairs_all <- app$wait_for_value(output = "summary_char-pairs_all")
  pairs_sub <- app$wait_for_value(output = "summary_char-pairs_sub")
  expect_match(pairs_all, "<table")
  expect_match(pairs_sub, "<table")

  network_dl <- app$get_download("summary_char-download_network")
  df <- read.csv(network_dl)
  expect_equal(ncol(df), 3) # names are now a column
  expect_equal(nrow(df), 9)
  expect_true(all(c("All.studies", "With.selected.studies.excluded") %in% colnames(df)[2:3]))

  trt_all_dl <- app$get_download("summary_char-download_treatments_all")
  df <- read.csv(trt_all_dl)
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), n_trt_all)

  trt_sub_dl <- app$get_download("summary_char-download_treatments_sub")
  df <- read.csv(trt_sub_dl)
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), n_trt_sub)

  pairs_all_dl <- app$get_download("summary_char-download_pairs_all")
  df <- read.csv(pairs_all_dl)
  expect_equal(ncol(df), 3)
  expect_equal(nrow(df), n_comparisons_all)

  pairs_sub_dl <- app$get_download("summary_char-download_pairs_sub")
  df <- read.csv(pairs_sub_dl)
  expect_equal(ncol(df), 3)
  expect_equal(nrow(df), n_comparisons_sub)

  app$stop()
})

