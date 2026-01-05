test_that("freq_summary produces functions correctly", {
  freq_data <- defined_data_con$freq_all
  result <- freq_forest(freq_data, "Placebo", "random", "OR", 0, 1, "title")

  expect_match(result, "<svg")
})

test_that("freq_summary produces errors for incorrect data types and invalid values", {

  freq_data <- defined_data_con$freq_all

  expect_error(freq_forest("not_a_list", "Reference_Treatment", "random", "OR", 0, 1, "title"), "freq must be of class list")
  expect_error(freq_forest(freq_data, 123, "random", "OR", 0, 1, "title"), "reference_treatment must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", 123, "OR", 0, 1, "title"), "model_type must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", 123, 0, 1, "title"), "outcome_measure must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "OR", "not_numeric", 1, "title"), "xmin must be of class numeric")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "OR", 0, "not_numeric", "title"), "xmax must be of class numeric")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "invalid_model_type", "OR", 0, 1, "title"), "model_type must be 'fixed' or 'random'")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "invalid_outcome_measure", 0, 1, "title"), "outcome_measure must be 'OR', 'RR', 'RD', 'MD' or 'SMD'")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "OR", 0, 1, 0), "title must be of class character")
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
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_forest")
  app$click("freq_forest-run")

  app$wait_for_value(output = "freq_forest-plot_all")
  app$wait_for_value(output = "freq_forest-plot_sub")
  plot_all <- app$get_value(output = "freq_forest-plot_all")
  plot_sub <- app$get_value(output = "freq_forest-plot_sub")
  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")

  test_plot_downloads(app, "freq_forest")

  app$stop()
})
