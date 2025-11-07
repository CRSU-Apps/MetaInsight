test_that("Check covariate_deviance function works as expected", {
  result <- covariate_deviance(fitted_covariate_model)
  expect_type(result, "list")
  expect_true(all(c("deviance_mtc", "stem_plot", "lev_plot") %in% names(result)))
  expect_is(result$deviance_mtc, "mtc.deviance")
  expect_is(result$stem_plot, "plotly")
  expect_is(result$lev_plot, "plotly")
})

test_that("Check covariate_deviance function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(covariate_deviance("faulty_model"), "model must be an object created by bayes_model")
  expect_error(covariate_deviance(list(a = 1)), "model must be an object created by bayes_model")
  expect_error(covariate_deviance(faulty_model), "model must be an object created by bayes_model")
})

test_that("{shinytest2} recording: e2e_covariate_deviance", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_deviance", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_deviance")
  app$click("covariate_deviance-run")
  app$wait_for_value(output = "covariate_deviance-covariate-stem")
  app$wait_for_value(output = "covariate_deviance-covariate-lev")

  stem <- app$get_value(output = "covariate_deviance-covariate-stem")
  lev <- app$get_value(output = "covariate_deviance-covariate-lev")

  expect_is(stem, "json")
  expect_is(lev, "json")

  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(stem))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(lev))))

  app$stop()
})

