test_that("freq_summary produces errors for incorrect data types and invalid values", {

  freq_data <- defined_data_con$freq_all
  treatment_data <- defined_data_con$treatment_df

  expect_error(freq_summary("not_a_list", treatment_data, "Title", "OR", "good", "random", 999), "freq must be of class list")
  expect_error(freq_summary(freq_data, "not_a_dataframe", "Title", "OR", "good", "random", 999), "treatment_df must be of class data.frame")
  expect_error(freq_summary(freq_data, treatment_data, 123, "OR", "good", "random", 999), "plot_title must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", 123, "good", "random", 999), "outcome_measure must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", 123, "random", 999), "ranking_option must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "good", 123, 999), "model_type must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "good", "random", "999"), "seed must be of class numeric")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "invalid_ranking_option", "random", 999), "ranking_option must be 'good' or 'bad'")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "invalid_outcome_measure", "good", "random", 999), "outcome_measure must be 'OR', 'RR', 'RD', 'MD' or 'SMD'")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "good", "invalid_model_type", 999), "model_type must be 'fixed' or 'random'")
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

  expect_equal(substr(plot_all$src, 1, 10), "data:image")
  expect_equal(substr(plot_sub$src, 1, 10), "data:image")

  test_plot_downloads(app, "freq_summary")

  app$stop()
})
