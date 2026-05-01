
test_that("Check bayes_model function works as expected", {
  result <- bayes_model(configured_data_con, 100, 100)

  expect_is(result, "bayes_model")
  expect_true(all(c("mtcResults",
                    "mtcRelEffects",
                    "rel_eff_tbl",
                    "sumresults",
                    "mtcNetwork",
                    "dic",
                    "outcome",
                    "outcome_measure",
                    "reference_treatment",
                    "effects",
                    "seed") %in% names(result)))
  expect_is(result$dic, "data.frame")
  expect_is(result$mtcResults, "mtc.result")
  expect_is(result$mtcRelEffects, "mtc.result")
  expect_is(result$rel_eff_tbl, "mtc.relative.effect.table")
  expect_is(result$mtcNetwork, "mtc.network")
  expect_is(result$sumresults, "summary.mtc.result")
  expect_is(result$outcome, "character")
  expect_is(result$outcome_measure, "character")
  expect_is(result$reference_treatment, "character")
  expect_is(result$effects, "character")
  expect_is(result$seed, "numeric")

  # check results are reproducible
  result_2 <- bayes_model(configured_data_con, 100, 100)
  expect_true(identical(remove_igraph(result), remove_igraph(result_2)))

})

test_that("bayes_model produces errors for incorrect data types", {
  expect_error(bayes_model("not_data"), "configured_data must be of class configured_data")

  invalid_outcome_measure <- configured_data_con
  invalid_outcome_measure$outcome_measure <- "SMD"
  expect_error(bayes_model(invalid_outcome_measure), "configured data must have an outcome_measure")

})

test_that("bayes_model works e2e - that models are initally different, update after exclusions and are then identical", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_model", timeout = 30000)
  reload_app(app, config_path)
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_model")
  app$click("bayes_model-run")

  app$wait_for_value(input = "bayes_model-all-complete")
  app$wait_for_value(input = "bayes_model-sub-complete")

  table_all <- app$wait_for_value(output = "bayes_model-all-table")
  table_sub <- app$wait_for_value(output = "bayes_model-sub-table")

  expect_match(table_all$html, "<table")
  expect_match(table_sub$html, "<table")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_model_all, "bayes_model")
  expect_is(common$bayes_model_sub, "bayes_model")

  expect_false(identical(remove_igraph(common$bayes_model_all), remove_igraph(common$bayes_model_sub)))

  app$click(selector = "#setup_exclude-collapse .accordion-button")
  app$wait_for_idle()
  click_setup_exclude(app, "Minerva")
  app$wait_for_value(input = "bayes_model-sub-updated")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_model_all, "bayes_model")
  expect_is(common$bayes_model_sub, "bayes_model")

  expect_true(identical(remove_igraph(common$bayes_model_all), remove_igraph(common$bayes_model_sub)))

  app$stop()
})

