test_that("summary_study produces errors for incorrect data types", {
  expect_error(summary_study("not_a_dataframe", "MD", 1, 1), "freq must be of class list")
  expect_error(summary_study(excluded_data_con$freq_sub, 123, 1, 1), "outcome_measure must be of class character")
  expect_error(summary_study(excluded_data_con$freq_sub, "MD", "AA", 1), "header must be of class numeric")
  expect_error(summary_study(excluded_data_con$freq_sub, "MD", 1, "AA"), "title must be of class numeric")
  expect_error(summary_study(excluded_data_con$freq_sub, "AA", 1, 1), "outcome_measure must be either MD, SMD, OR, RR or RD")
})

# add something to test the production of the plot

test_that("summary_study produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$set_inputs(summarySel = "summary_study")
  app$click("summary_study-run")

  test_plot_downloads(app, "summary_study", pair = FALSE)

  app$stop()
})

