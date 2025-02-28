test_that("freq_summary produces errors for incorrect data types and invalid values", {

  freq_data <- defined_data_con$freq_all

  expect_error(freq_forest("not_a_list", "Reference_Treatment", "random", "OR", 0, 1), "freq must be of class list")
  expect_error(freq_forest(freq_data, 123, "random", "OR", 0, 1), "reference_treatment must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", 123, "OR", 0, 1), "model_type must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", 123, 0, 1), "outcome_measure must be of class character")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "OR", "not_numeric", 1), "xmin must be of class numeric")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "OR", 0, "not_numeric"), "xmax must be of class numeric")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "invalid_model_type", "OR", 0, 1), "model_type must be 'fixed' or 'random'")
  expect_error(freq_forest(freq_data, "Reference_Treatment", "random", "invalid_outcome_measure", 0, 1), "outcome_measure must be 'OR', 'RR', 'RD', 'MD' or 'SMD'")

})

test_that("summary_network produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "summary_exclude-complete")
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_forest")
  app$click("freq_forest-run")

  # don't know why, but the downloads fail without this
  common <- app$get_value(export = "common")

  test_plot_downloads(app, "freq_forest")

  app$stop()
})
