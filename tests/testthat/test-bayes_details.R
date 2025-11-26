test_that("Check bayes_details function works as expected", {
  result <- bayes_details(fitted_bayes_model)
  expect_type(result, "list")
  expect_true(all(c("mcmc", "priors") %in% names(result)))
  expect_is(result$mcmc, "data.frame")
  expect_is(result$priors, "data.frame")
  expect_equal(ncol(result$mcmc), 2)
  expect_equal(nrow(result$mcmc), 4)
  expect_equal(ncol(result$priors), 2)
  expect_equal(nrow(result$priors), 3)
})

test_that("Check bayes_details function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(bayes_details("faulty_model"), "model must be an object created by baseline_model")
  expect_error(bayes_details(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(bayes_details(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_bayes_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_details", timeout = 30000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "bayes")

  app$set_inputs(bayesSel = "bayes_deviance")
  app$click("bayes_deviance-run")
  app$wait_for_value(input = "bayes_deviance-complete")

  app$set_inputs(bayesSel = "bayes_details")
  app$click("bayes_details-run")

  app$wait_for_idle()

  mcmc <- app$wait_for_value(output = "bayes_details-bayes-mcmc")
  priors <- app$wait_for_value(output = "bayes_details-bayes-priors")
  expect_match(mcmc, "<table")
  expect_match(priors, "<table")
  mcmc_dl <- app$get_download("bayes_details-bayes-download_mcmc")
  expect_equal(nrow(read.csv(mcmc_dl)), 4)
  priors_dl <- app$get_download("bayes_details-bayes-download_priors")
  expect_equal(nrow(read.csv(priors_dl)), 3)

  app$set_inputs("bayes_details-bayes-tabs" = "code")
  code <- app$wait_for_value(output = "bayes_details-bayes-code")
  expect_match(code, "model")
  code_dl <- app$get_download("bayes_details-bayes-download_code")
  expect_gt(length(readLines(code_dl)), 10)

  app$set_inputs("bayes_details-bayes-tabs" = "sims")
  for (n in 1:4){
    sim <- app$get_download(glue::glue("bayes_details-bayes-download_data_{n}"))
    expect_gt(nrow(read.csv(sim)), 10)
  }

  app$set_inputs("bayes_details-bayes-tabs" = "inits")
  inits <- app$wait_for_value(output = "bayes_details-bayes-inits")
  expect_match(inits, "delta")
  for (n in 1:4){
    init <- app$get_download(glue::glue("bayes_details-bayes-download_inits_{n}"))
    expect_gt(length(readLines(init)), 10)
  }

  app$set_inputs("bayes_details-bayes-tabs" = "dev")
  dev_mtc_all <- app$wait_for_value(output = "bayes_details-bayes-dev_mtc")
  dev_ume_all <- app$wait_for_value(output = "bayes_details-bayes-dev_ume")
  dev_mtc_sub <- app$wait_for_value(output = "bayes_details-bayes-dev_mtc_sub")
  dev_ume_sub <- app$wait_for_value(output = "bayes_details-bayes-dev_ume_sub")

  expect_match(dev_mtc_all, "dev\\.ab")
  expect_match(dev_ume_all, "dev\\.ab")
  expect_match(dev_mtc_sub, "dev\\.ab")
  expect_match(dev_ume_sub, "dev\\.ab")

  app$stop()
})

