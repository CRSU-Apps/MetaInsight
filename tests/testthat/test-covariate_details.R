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

test_that("{shinytest2} recording: e2e_covariate_details", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_details", timeout = 30000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "covariate")

  app$set_inputs(covariateSel = "covariate_deviance")
  app$click("covariate_deviance-run")
  app$wait_for_value(output = "covariate_deviance-covariate-stem")

  app$set_inputs(covariateSel = "covariate_details")
  app$click("covariate_details-run")

  app$wait_for_value(output = "covariate_details-covariate-mcmc")
  app$wait_for_value(output = "covariate_details-covariate-priors")
  app$set_inputs("covariate_details-covariate-tabs" = "code")
  app$wait_for_value(output = "covariate_details-covariate-code")
  app$set_inputs("covariate_details-covariate-tabs" = "inits")
  app$wait_for_value(output = "covariate_details-covariate-inits")
  app$set_inputs("covariate_details-covariate-tabs" = "dev")
  app$wait_for_value(output = "covariate_details-covariate-dev_mtc")

  mcmc <- app$get_value(output = "covariate_details-covariate-mcmc")
  priors <- app$get_value(output = "covariate_details-covariate-priors")
  code <- app$get_value(output = "covariate_details-covariate-code")
  inits <- app$get_value(output = "covariate_details-covariate-inits")
  dev_mtc <- app$get_value(output = "covariate_details-covariate-dev_mtc")

  expect_match(mcmc, "<table")
  expect_match(priors, "<table")
  expect_match(code, "model")
  expect_match(inits, "Wichmann-Hill")
  expect_match(dev_mtc, "dev\\.ab")
})

