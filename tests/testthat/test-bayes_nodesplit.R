connected <- defined_data_con$main_connected_data
t_df <- defined_data_con$treatment_df

loaded_ns <- setup_load(data_path = file.path(test_data_dir, "Cont_nodesplit.csv"), outcome = "Continuous")
defined_ns <- setup_configure(loaded_ns$data, loaded_ns$treatment_df, "Continuous", "MD", "Placebo")

test_that("Check bayes_nodesplit function works as expected", {
  result <- bayes_nodesplit(defined_ns$main_connected_data, defined_ns$treatment_df, "Continuous", "MD", "random")
  expect_is(result, "mtc.nodesplit")
  expect_length(result, 9)
  expect_true(all(unlist(lapply(result, class)) == "mtc.result"))
})

test_that("bayes_nodesplit produces errors for incorrect data types", {
  expect_error(bayes_nodesplit("not_a_dataframe", t_df, "Continuous", "MD", "random"), "connected_data must be of class data.frame")
  expect_error(bayes_nodesplit(connected, "not_a_dataframe", "Continuous", "MD", "random"), "treatment_df must be of class data.frame")
  expect_error(bayes_nodesplit(connected, t_df, 123, "MD", "random"), "outcome must be of class character")
  expect_error(bayes_nodesplit(connected, t_df, "Continuous", 123, "random"), "outcome_measure must be of class character")
  expect_error(bayes_nodesplit(connected, t_df, "Continuous", "MD", 123), "model_type must be of class character")
  expect_error(bayes_nodesplit(connected, t_df, "invalid_outcome", "MD", "random"), "outcome must be either Binary or Continuous")
  expect_error(bayes_nodesplit(connected, t_df, "Continuous", "invalid_measure", "random"), "Outcome_measure type 'invalid_measure' is not supported")
  expect_error(bayes_nodesplit(connected, t_df, "Continuous", "MD", "not_random"), "model_type must be 'fixed' or 'random'")
})

test_that("bayes_nodesplit produces errors for data which cannot be split", {
  expect_error(bayes_nodesplit(connected, t_df, "Continuous", "MD", "random"), "There are no loops in the network")
})

test_that("bayes_nodesplit functions with valid data", {
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

test_that("bayes_nodesplit returns an error with invalid data", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_nodesplit", timeout = 60000)
  app$click("setup_load-run")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_nodesplit")
  app$click("bayes_nodesplit-run")
  app$wait_for_value(input = "bayes_nodesplit-all-complete")
  app$wait_for_value(input = "bayes_nodesplit-sub-complete")

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*There are no loops in the network*", logger))
  app$stop()
})
