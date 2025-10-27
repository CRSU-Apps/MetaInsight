test_that("Check bayes_deviance function works as expected", {
  result <- bayes_deviance(fitted_bayes_model)
  expect_type(result, "list")
  expect_true(all(c("deviance_mtc", "deviance_ume", "scat_plot", "stem_plot", "lev_plot") %in% names(result)))
  expect_is(result$deviance_mtc, "mtc.deviance")
  expect_is(result$deviance_ume, "mtc.deviance")
  expect_is(result$scat_plot, "plotly")
  expect_is(result$stem_plot, "plotly")
  expect_is(result$lev_plot, "plotly")
})

test_that("Check bayes_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(bayes_deviance("faulty_model"), "model must be an object created by bayes_model() or covariate_model()")
  expect_error(bayes_deviance(list(a = 1)), "model must be an object created by bayes_model() or covariate_model()")
  expect_error(bayes_deviance(faulty_model), "model must be an object created by bayes_model() or covariate_model()")
})

test_that("{shinytest2} recording: e2e_bayes_deviance", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_deviance", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_deviance")
  app$click("bayes_deviance-run")
  app$wait_for_value(output = "bayes_deviance-all-scat")
  app$wait_for_value(output = "bayes_deviance-sub-scat")
  app$wait_for_value(output = "bayes_deviance-all-stem")
  app$wait_for_value(output = "bayes_deviance-sub-stem")
  app$wait_for_value(output = "bayes_deviance-all-lev")
  app$wait_for_value(output = "bayes_deviance-sub-lev")

  scat_all <- app$get_value(output = "bayes_deviance-all-scat")
  scat_sub <- app$get_value(output = "bayes_deviance-sub-scat")
  stem_all <- app$get_value(output = "bayes_deviance-all-stem")
  stem_sub <- app$get_value(output = "bayes_deviance-sub-stem")
  lev_all <- app$get_value(output = "bayes_deviance-all-lev")
  lev_sub <- app$get_value(output = "bayes_deviance-sub-lev")

  expect_is(scat_all, "json")
  expect_is(scat_sub, "json")
  expect_is(stem_all, "json")
  expect_is(stem_sub, "json")
  expect_is(lev_all, "json")
  expect_is(lev_sub, "json")

  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(scat_all))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(scat_sub))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(stem_all))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(stem_sub))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(lev_all))))
  expect_true(all(c("x", "evals", "jsHooks", "deps") %in% names(jsonlite::fromJSON(lev_sub))))

  app$stop()
})

