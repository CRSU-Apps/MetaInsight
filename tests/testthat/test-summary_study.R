test_that("summary_study produces functions correctly", {
  result_1 <- summary_study(configured_data_con)

  expect_match(result_1, "<svg")

  result_2 <- summary_study(configured_data_con, 7)
  result_3 <- summary_study(configured_data_con, 6, TRUE)
  result_4 <- summary_study(configured_data_con, 6, FALSE, 0, 1)
  result_5 <- summary_study(configured_data_bin)
  expect_match(result_5, "<svg")

  # should all be different
  expect_false(identical(result_1, result_2))
  expect_false(identical(result_1, result_3))
  expect_false(identical(result_2, result_3))
  expect_false(identical(result_1, result_4))
  expect_false(identical(result_2, result_4))
  expect_false(identical(result_3, result_4))
})


test_that("summary_study produces errors for incorrect data types", {
  expect_error(summary_study("not_data", 6, TRUE, 0, 1), "configured_data must be of class configured_data")
  expect_error(summary_study(configured_data_con, "123", TRUE, 0, 1), "plot_area_width must be of class numeric")
  expect_error(summary_study(configured_data_con, 6, "TRUE", 0, 1), "colourblind must be of class logical")
  expect_error(summary_study(configured_data_con, 6, TRUE, "123", 1), "x_min must be of class numeric")
  expect_error(summary_study(configured_data_con, 6, TRUE, 1, "123"), "x_max must be of class numeric")

  expect_error(summary_study(configured_data_con, 5, TRUE, 0, 1), "plot_area_width must be between 6 and 20")
  expect_error(summary_study(configured_data_con, 21, TRUE, 0, 1), "plot_area_width must be between 6 and 20")
})

test_that("summary_study produces downloadable plots", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_summary", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(summarySel = "summary_study")
  app$click("summary_study-run")
  app$wait_for_value(output = "summary_study-plot")
  plot <- app$get_value(output = "summary_study-plot")
  expect_match(plot$html, "<svg")
  test_plot_downloads(app, "summary_study", pair = FALSE)
  app$stop()
})

