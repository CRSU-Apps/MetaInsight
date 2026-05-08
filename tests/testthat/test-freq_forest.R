test_that("freq_summary produces functions correctly", {
  result <- freq_forest(configured_data_con, 0, 1, "title")
  expect_match(result, "<svg")

  result <- freq_forest(configured_data_bin, 0.01, 100, "title")
  expect_match(result, "<svg")
})

test_that("freq_summary produces errors for incorrect data types and invalid values", {
  expect_error(freq_forest("not_data", 0, 1, "title"), "configured_data must be of class configured_data")
  expect_error(freq_forest(configured_data_con, "not_numeric", 1, "title"), "xmin must be of class numeric")
  expect_error(freq_forest(configured_data_con, 0, "not_numeric", "title"), "xmax must be of class numeric")
  expect_error(freq_forest(configured_data_con, 0, 1, 0), "title must be of class character")
})

test_that("summary_network produces downloadable plots", {
  skip_if(skip_shinytest2)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load", timeout = 30000)
  reload_app(app, config_path)
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_forest")
  app$click("freq_forest-run")

  app$wait_for_value(output = "freq_forest-plot_all")
  app$wait_for_value(output = "freq_forest-plot_sub")
  plot_all <- app$get_value(output = "freq_forest-plot_all")
  plot_sub <- app$get_value(output = "freq_forest-plot_sub")
  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")
  expect_false(identical(plot_all$html, plot_sub$html))

  test_plot_downloads(app, "freq_forest")

  app$stop()
})
