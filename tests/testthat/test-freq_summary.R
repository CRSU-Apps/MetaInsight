test_that("freq_summary produces errors for incorrect data types and invalid values", {

  freq_data <- defined_data_con$freq_all
  treatment_data <- defined_data_con$treatment_df

  expect_error(freq_summary("not_a_list", treatment_data, "Title", "OR", "good", "random"), "freq must be of class list")
  expect_error(freq_summary(freq_data, "not_a_dataframe", "Title", "OR", "good", "random"), "treatment_df must be of class data.frame")
  expect_error(freq_summary(freq_data, treatment_data, 123, "OR", "good", "random"), "plot_title must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", 123, "good", "random"), "outcome_measure must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", 123, "random"), "ranking_option must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "good", 123), "model_type must be of class character")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "invalid_ranking_option", "random"), "ranking_option must be 'good' or 'bad'")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "invalid_outcome_measure", "good", "random"), "outcome_measure must be 'OR', 'RR', 'RD', 'MD' or 'SMD'")
  expect_error(freq_summary(freq_data, treatment_data, "Title", "OR", "good", "invalid_model_type"), "model_type must be 'fixed' or 'random'")
})

test_that("summary_network produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_summary")
  app$click("freq_summary-run")

  # don't know why, but the downloads fail without this
  common <- app$get_value(export = "common")

  test_plot_downloads(app, "freq_summary")

  app$stop()
})
