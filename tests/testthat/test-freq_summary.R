test_that("freq_summary produces errors for incorrect data types and invalid values", {
  result <- freq_summary(configured_data_con, "Title")
  expect_match(result, "<svg")

  result <- freq_summary(configured_data_bin, "Title")
  expect_match(result, "<svg")
})

test_that("freq_summary produces errors for incorrect data types and invalid values", {
  expect_error(freq_summary("not_data", "Title"), "configured_data must be of class configured_data")
  expect_error(freq_summary(configured_data_con, 123), "plot_title must be of class character")
})

test_that("freq_summary produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_summary")
  app$click("freq_summary-run")

  plot_all <- app$wait_for_value(output = "freq_summary-plot_all")
  plot_sub <- app$wait_for_value(output = "freq_summary-plot_sub")

  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")

  test_plot_downloads(app, "freq_summary")

  app$stop()
})
