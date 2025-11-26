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

test_that("Check baseline_details function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(baseline_details("faulty_model"), "model must be an object created by baseline_model")
  expect_error(baseline_details(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(baseline_details(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_baseline_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_details", timeout = 30000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_deviance")
  app$click("baseline_deviance-run")
  app$wait_for_value(input = "baseline_deviance-complete")

  app$set_inputs(baselineSel = "baseline_details")
  app$click("baseline_details-run")

  app$wait_for_idle()
  mcmc <- app$wait_for_value(output = "baseline_details-baseline-mcmc")
  expect_match(mcmc, "<table")
  priors <- app$wait_for_value(output = "baseline_details-baseline-priors")
  expect_match(priors, "<table")
  mcmc_dl <- app$get_download("baseline_details-baseline-download_mcmc")
  expect_equal(nrow(read.csv(mcmc_dl)), 4)
  priors_dl <- app$get_download("baseline_details-baseline-download_priors")
  expect_equal(nrow(read.csv(priors_dl)), 4)

  app$set_inputs("baseline_details-baseline-tabs" = "sims")
  for (n in 1:4){
    sim <- app$get_download(glue::glue("baseline_details-baseline-download_data_{n}"))
    expect_gt(nrow(read.csv(sim)), 10)
  }

  app$set_inputs("baseline_details-baseline-tabs" = "code")
  code <- app$wait_for_value(output = "baseline_details-baseline-code")
  expect_match(code, "model")
  code_dl <- app$get_download("baseline_details-baseline-download_code")
  # this gives a warning about an incomplete line, but opens fine
  code_dl_lines <- suppressWarnings(readLines(code_dl))
  expect_gt(length(code_dl_lines), 10)

  app$set_inputs("baseline_details-baseline-tabs" = "inits")
  inits <- app$wait_for_value(output = "baseline_details-baseline-inits")
  expect_match(inits, "Wichmann-Hill")
  for (n in 1:4){
    init <- app$get_download(glue::glue("baseline_details-baseline-download_inits_{n}"))
    expect_equal(length(readLines(init)), 8)
  }

  app$set_inputs("baseline_details-baseline-tabs" = "dev")
  app$wait_for_value(output = "baseline_details-baseline-dev_mtc")
  dev_mtc <- app$wait_for_value(output = "baseline_details-baseline-dev_mtc")
  expect_match(dev_mtc, "dev\\.ab")

  app$stop()
})

