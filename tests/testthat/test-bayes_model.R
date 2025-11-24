connected <- defined_data_con$main_connected_data
t_df <- defined_data_con$treatment_df

test_that("Check bayes_model function works as expected", {
  result <- bayes_model(connected, t_df, "Continuous", "MD", "random", "Placebo", 123)

  expect_is(result, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "sumresults",
                    "mtcNetwork",
                    "dic",
                    "outcome_measure",
                    "model_type") %in% names(result)))
  expect_is(result$dic, "data.frame")
  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$sumresults, "summary.mtc.result")
  expect_is(result$outcome_measure, "character")
  expect_is(result$model_type, "character")

  # check results are reproducible
  result_2 <- bayes_model(connected, t_df, "Continuous", "MD", "random", "Placebo", 123)
  expect_true(identical(remove_igraph(result_1), remove_igraph(result_2)))

})

test_that("bayes_model produces errors for incorrect data types", {
  expect_error(bayes_model("not_a_dataframe", t_df, "Continuous", "MD", "random", "Placebo", 123), "connected_data must be of class data.frame")
  expect_error(bayes_model(connected, "not_a_dataframe", "Continuous", "MD", "random", "Placebo", 123), "treatment_df must be of class data.frame")
  expect_error(bayes_model(connected, t_df, 123, "MD", "random", "Placebo", 123), "outcome must be of class character")
  expect_error(bayes_model(connected, t_df, "Continuous", 123, "random", "Placebo", 123), "outcome_measure must be of class character")
  expect_error(bayes_model(connected, t_df, "Continuous", "MD", 123, "Placebo", 123), "model_type must be of class character")
  expect_error(bayes_model(connected, t_df, "Continuous", "MD", "random", 123, 123), "reference_treatment must be of class character")
  expect_error(bayes_model(connected, t_df, "Continuous", "MD", "random", "Placebo", "not seed"), "seed must be of class numeric")
  expect_error(bayes_model(connected, t_df, "invalid_outcome", "MD", "random", "Placebo", 123), "outcome must be either Binary or Continuous")
  expect_error(bayes_model(connected, t_df, "Continuous", "invalid_measure", "random", "Placebo", 123), "outcome_measure must be 'OR', 'RR' or 'MD'")
  expect_error(bayes_model(connected, t_df, "Continuous", "MD", "not_random", "Placebo", 123), "model_type must be 'fixed' or 'random'")
  expect_error(bayes_model(connected, t_df, "Continuous", "MD", "random", "not_a_placebo", 123), "reference_treatment must be one of the treatments in treatment_df")
})

test_that("bayes_model works e2e - that models are initally identical, update after exclusions and are then different", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_model", timeout = 30000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
  app$click("setup_load-run")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_model")
  app$click("bayes_model-run")

  app$wait_for_value(input = "bayes_model-all-complete")
  app$wait_for_value(input = "bayes_model-sub-complete")

  table_all <- app$wait_for_value(output = "bayes_model-all-table")
  table_sub <- app$wait_for_value(output = "bayes_model-sub-table")

  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_all, "bayes_model")
  expect_is(common$bayes_sub, "bayes_model")

  expect_true(identical(remove_igraph(common$bayes_all), remove_igraph(common$bayes_sub)))

  app$set_inputs("setup_exclude-exclusions" = "Leo")
  app$wait_for_value(input = "bayes_model-sub-updated")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_all, "bayes_model")
  expect_is(common$bayes_sub, "bayes_model")

  expect_false(identical(remove_igraph(common$bayes_all), remove_igraph(common$bayes_sub)))

  app$stop()
})

