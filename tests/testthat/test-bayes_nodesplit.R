test_that("Check bayes_nodesplit function works as expected", {
  loaded_data <- setup_load(data_path = file.path(test_data_dir, "Cont_nodesplit.csv"), outcome = "Continuous")
  defined_data <- setup_configure(loaded_data$data, loaded_data$treatment_df, "Continuous", "MD", "Placebo")
  result <- bayes_nodesplit(defined_data$main_connected_data, defined_data$treatment_df, "Continuous", "MD", "random")
  expect_is(result, "mtc.nodesplit")
  expect_length(result, 9)
  expect_true(all(unlist(lapply(result, class)) == "mtc.result"))
})

test_that("{shinytest2} recording: e2e_bayes_nodesplit", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_nodesplit", timeout = 60000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_nodesplit.csv"))
  app$click("setup_load-run")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_nodesplit")
  app$click("bayes_nodesplit-run")
  app$wait_for_value(input = "bayes_nodesplit-all-complete")
  app$wait_for_value(input = "bayes_nodesplit-sub-complete")

  common <- app$get_value(export = "common")
  expect_is(common$nodesplit_all, "mtc.nodesplit")
  expect_is(common$nodesplit_sub, "mtc.nodesplit")

  plot_all <- app$get_value(output = "bayes_nodesplit-all-plot")
  plot_sub <- app$get_value(output = "bayes_nodesplit-sub-plot")
  expect_equal(substr(plot_all$src, 1, 10), "data:image")
  expect_equal(substr(plot_sub$src, 1, 10), "data:image")

  test_bayes_plot_downloads(app, "bayes_nodesplit", "")

  app$stop()
})

