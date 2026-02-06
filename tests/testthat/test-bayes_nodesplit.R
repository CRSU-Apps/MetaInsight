loaded_ns <- setup_load(data_path = file.path(test_data_dir, "Cont_nodesplit.csv"), outcome = "continuous")
configured_ns <- setup_configure(loaded_ns, "Placebo", "random", "MD", "good", 123)

test_that("Check bayes_nodesplit function works as expected", {
  result <- bayes_nodesplit(configured_ns)
  expect_is(result, "mtc.nodesplit")
  expect_length(result, 9)
  expect_true(all(unlist(lapply(result, class)) == "mtc.result"))

  plot <- bayes_nodesplit_plot(result)
  expect_match(plot, "<svg")

})

test_that("bayes_nodesplit produces errors for incorrect data types", {
  expect_error(bayes_nodesplit("not_data"), "configured_data must be of class configured_data")
})

test_that("bayes_nodesplit produces errors for data which cannot be split", {
  expect_error(bayes_nodesplit(configured_data_con), "There are no loops in the network")
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

  expect_match(plot_all$html, "<svg")
  expect_match(plot_sub$html, "<svg")

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
