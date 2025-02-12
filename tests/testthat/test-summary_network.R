test_that("summary_exclude produces errors for incorrect data types", {
  expect_error(summary_network("not_a_list", defined_data$bugsnet_all, "netgraph", 1), "freq must be of class list")
  expect_error(summary_network(defined_data_con$freq_all, "not_a_dataframe", "netgraph", 1), "bugsnet must be of class data.frame")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, 123, 1), "style must be of class character")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, "netgraph", "not_a_number"), "label_size must be of class numeric")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, "invalid_style", 1), "style must be either netgraph or netplot")
})

# Add something to test plot creation

test_that("{shinytest2} recording: e2e_summary_network", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$set_inputs(summarySel = "summary_network")
  app$set_inputs(main = "Results")
  pdf_all <- app$get_download("summary_network-download_all")
  pdf_sub <- app$get_download("summary_network-download_sub")
  expect_gt(file.info(pdf_all)$size, 1000)
  expect_gt(file.info(pdf_sub)$size, 1000)
  app$set_inputs("summary_network-format_all" = "PNG")
  app$set_inputs("summary_network-format_sub" = "PNG")
  png_all <- app$get_download("summary_network-download_all")
  png_sub <- app$get_download("summary_network-download_sub")
  expect_gt(file.info(png_all)$size, 1000)
  expect_gt(file.info(png_sub)$size, 1000)
  app$stop()
})

