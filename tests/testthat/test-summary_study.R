test_that("summary_study produces errors for incorrect data types", {
  expect_error(summary_study("not_a_dataframe", "MD", 1, 1), "freq must be of class list")
  expect_error(summary_study(excluded_data_con$freq_sub, 123, 1, 1), "outcome_measure must be of class character")
  expect_error(summary_study(excluded_data_con$freq_sub, "MD", "AA", 1), "header must be of class numeric")
  expect_error(summary_study(excluded_data_con$freq_sub, "MD", 1, "AA"), "title must be of class numeric")
  expect_error(summary_study(excluded_data_con$freq_sub, "AA", 1, 1), "outcome_measure must be either MD, SMD, OR, RR or RD")
})

# add something to test the production of the plot

test_that("summary_study produces a downloadable file", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$set_inputs(summarySel = "summary_study")
  app$set_inputs(main = "Results")
  plot_pdf <- app$get_download("summary_study-download")
  expect_gt(file.info(plot_pdf)$size, 1000)
  app$set_inputs("summary_study-format" = "svg")
  plot_svg <- app$get_download("summary_study-download")
  expect_gt(file.info(plot_svg)$size, 1000)

  app$stop()
})

