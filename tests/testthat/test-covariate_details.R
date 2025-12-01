test_that("Check covariate_details function works as expected", {
  result <- covariate_details(fitted_covariate_model)
  expect_type(result, "list")
  expect_true(all(c("mcmc", "priors") %in% names(result)))
  expect_is(result$mcmc, "data.frame")
  expect_is(result$priors, "data.frame")
  expect_equal(ncol(result$mcmc), 2)
  expect_equal(nrow(result$mcmc), 4)
  expect_equal(ncol(result$priors), 2)
  expect_equal(nrow(result$priors), 4)
})

test_that("Check covariate_details function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(covariate_details("faulty_model"), "model must be an object created by baseline_model")
  expect_error(covariate_details(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(covariate_details(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_covariate_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_details", timeout = 30000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "covariate")

  app$set_inputs(covariateSel = "covariate_deviance")
  app$click("covariate_deviance-run")
  app$wait_for_value(input = "covariate_deviance-complete")

  app$set_inputs(covariateSel = "covariate_details")
  app$click("covariate_details-run")

  app$wait_for_idle()
  mcmc <- app$wait_for_value(output = "covariate_details-covariate-mcmc")
  expect_match(mcmc, "<table")
  priors <- app$wait_for_value(output = "covariate_details-covariate-priors")
  expect_match(priors, "<table")
  mcmc_dl <- app$get_download("covariate_details-covariate-download_mcmc")
  expect_equal(nrow(read.csv(mcmc_dl)), 4)
  priors_dl <- app$get_download("covariate_details-covariate-download_priors")
  expect_equal(nrow(read.csv(priors_dl)), 4)

  app$set_inputs("covariate_details-covariate-tabs" = "sims")
  for (n in 1:4){
    sim <- app$get_download(glue::glue("covariate_details-covariate-download_data_{n}"))
    expect_gt(nrow(read.csv(sim)), 10)
  }

  app$set_inputs("covariate_details-covariate-tabs" = "code")
  code <- app$wait_for_value(output = "covariate_details-covariate-code")
  expect_match(code, "model")
  code_dl <- app$get_download("covariate_details-covariate-download_code")
  expect_gt(length(readLines(code_dl)), 10)

  app$set_inputs("covariate_details-covariate-tabs" = "inits")
  inits <- app$wait_for_value(output = "covariate_details-covariate-inits")
  expect_match(inits, "Wichmann-Hill")
  for (n in 1:4){
    init <- app$get_download(glue::glue("covariate_details-covariate-download_inits_{n}"))
    expect_gt(length(readLines(init)), 10)
  }

  app$set_inputs("covariate_details-covariate-tabs" = "dev")
  app$wait_for_value(output = "covariate_details-covariate-dev_mtc")
  dev_mtc <- app$wait_for_value(output = "covariate_details-covariate-dev_mtc")
  expect_match(dev_mtc, "dev\\.ab")

  app$stop()
})

