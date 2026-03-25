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
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  reload_app(app, config_path)
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_summary")
  app$click("freq_summary-run")

  plot_all <- app$wait_for_value(output = "freq_summary-plot_all")
  plot_sub <- app$wait_for_value(output = "freq_summary-plot_sub")

  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")
  expect_false(identical(plot_all$html, plot_sub$html))

  test_plot_downloads(app, "freq_summary")

  app$stop()
})
