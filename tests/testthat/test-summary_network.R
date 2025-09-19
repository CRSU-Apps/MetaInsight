test_that("summary_network produces svg plot", {
  freq <- defined_data_con$freq_all
  bugsnet <- defined_data_con$bugsnet_all
  result <- summary_network(freq, bugsnet, "netgraph", 1, "title")

  expect_match(result$svg, "<svg")
  expect_gt(result$width, 100)
  expect_lt(result$width, 1000)
  expect_gt(result$height, 100)
  expect_lt(result$height, 1000)

  result <- summary_network(freq, bugsnet, "netplot", 1, "title")

  expect_match(result$svg, "<svg")
  expect_gt(result$width, 100)
  expect_lt(result$width, 1000)
  expect_gt(result$height, 100)
  expect_lt(result$height, 1000)

})

test_that("summary_network produces errors for incorrect data types", {
  expect_error(summary_network("not_a_list", defined_data$bugsnet_all, "netgraph", 1, "title"), "freq must be of class list")
  expect_error(summary_network(defined_data_con$freq_all, "not_a_dataframe", "netgraph", 1, "title"), "bugsnet must be of class data.frame")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, 123, 1, "title"), "style must be of class character")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, "netgraph", "not_a_number", "title"), "label_size must be of class numeric")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, "invalid_style", 1, "title"), "style must be either netgraph or netplot")
  expect_error(summary_network(defined_data_con$freq_all, defined_data_con$bugsnet_all, "netgraph", 1, 123), "title must be of class character")
})

test_that("summary_network produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(summarySel = "summary_network")
  app$set_inputs(main = "Results")
  app$click("summary_network-run")

  app$wait_for_value(output = "summary_network-plot_all")
  app$wait_for_value(output = "summary_network-plot_sub")

  plot_all <- app$get_value(output = "summary_network-plot_all")
  plot_sub <- app$get_value(output = "summary_network-plot_sub")

  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")

  test_plot_downloads(app, "summary_network")

  app$stop()
})

