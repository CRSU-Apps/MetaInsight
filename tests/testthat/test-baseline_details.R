test_that("Check baseline_details function works as expected", {
  result <- baseline_details(fitted_baseline_model)
  expect_type(result, "list")
  expect_true(all(c("mcmc", "priors") %in% names(result)))
  expect_is(result$mcmc, "data.frame")
  expect_is(result$priors, "data.frame")
  expect_equal(ncol(result$mcmc), 2)
  expect_equal(nrow(result$mcmc), 4)
  expect_equal(ncol(result$priors), 2)
  expect_equal(nrow(result$priors), 4)
})

test_that("{shinytest2} recording: e2e_baseline_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_details")

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")

  app$set_inputs(baselineSel = "baseline_deviance")
  app$click("baseline_deviance-run")
  app$wait_for_value(output = "baseline_deviance-baseline-scat")

  app$set_inputs(baselineSel = "baseline_details")
  app$click("baseline_details-run")

  app$wait_for_value(output = "baseline_results-baseline-mcmc")
  app$wait_for_value(output = "baseline_results-baseline-priors")
  app$wait_for_value(output = "baseline_results-baseline-code")
  app$wait_for_value(output = "baseline_results-baseline-inits")
  app$wait_for_value(output = "baseline_results-baseline-dev_mtc")

  mcmc <- app$get_value(output = "baseline_results-baseline-mcmc")
  priors <- app$get_value(output = "baseline_results-baseline-priors")
  code <- app$get_value(output = "baseline_results-baseline-code")
  inits <- app$get_value(output = "baseline_results-baseline-inits")
  dev_mtc <- app$get_value(output = "baseline_results-baseline-dev_mtc")

  # need to add more
  expect_match(mcmc, "<table")
  expect_match(priors, "<table")

})

