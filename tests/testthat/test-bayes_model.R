test_that("Check bayes_model function works as expected", {
  result <- bayes_model(defined_data_con$main_connected_data, loaded_data_con$treatment_df, "Continuous", "MD", "random", "Placebo")

  expect_type(result, "list")
  expect_true(all(c("mtcResults", "mtcRelEffects", "rel_eff_tbl", "sumresults", "mtcNetwork", "dic") %in% names(result)))
  expect_is(result$dic, "data.frame")
  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$sumresults, "summary.mtc.result")
})

test_that("{shinytest2} recording: e2e_bayes_model", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_model", timeout = 30000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
  app$click("setup_load-run")
  app$click("setup_configure-run")
  app$set_inputs("setup_exclude-exclusions" = "Leo")
  app$wait_for_value(input = "setup_exclude-complete")
  app$click("bayes_model-run")
  app$wait_for_value(input = "bayes_model-all-complete")
  app$wait_for_value(input = "bayes_model-sub-complete")
  common <- app$get_value(export = "common")
  expect_type(common$bayes_all, "list")
  expect_type(common$bayes_sub, "list")
  app$stop()
})

