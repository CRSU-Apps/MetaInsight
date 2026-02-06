test_that("Check baseline_summary function works as expected", {
  result <- baseline_summary(configured_data_con)
  expect_match(result, "<svg")
})

test_that("Check baseline_summary function produces errors as expected", {
  expect_error(baseline_summary("not_data"), "configured_data must be of class configured_data")
})

test_that("{shinytest2} recording: e2e_baseline_summary", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_summary", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_summary")
  app$click("baseline_summary-run")

  app$wait_for_value(output = "baseline_summary-plot")
  plot <- app$get_value(output = "baseline_summary-plot")
  expect_match(plot$html, "<svg")

  test_plot_downloads(app, "baseline_summary", FALSE)

  app$stop()

})

