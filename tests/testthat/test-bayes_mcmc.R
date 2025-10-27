test_that("Check bayes_mcmc function works as expected", {
  result <- bayes_mcmc(fitted_bayes_model)
  expect_type(result, "list")
  expect_true(all(c("parameters", "gelman_data", "n_cols", "n_rows", "n_rows_rmd") %in% names(result)))

  expect_is(result$parameters, "character")
  expect_is(result$gelman_data, "list")
  expect_is(result$n_cols, "numeric")
  expect_is(result$n_rows, "numeric")
  expect_is(result$n_rows_rmd, "numeric")

  gelman <- gelman_plots(result$gelman_data, result$parameters)
  trace <- trace_plot(fitted_bayes_model$mtcResults, result$parameters)
  density <- density_plot(fitted_bayes_model$mtcResults, result$parameters)

  expect_is(gelman[[1]], "ggplot")
  expect_is(trace[[1]], "ggplot")
  expect_is(density[[1]], "ggplot")

  expect_length(gelman, 4)
  expect_length(trace, 4)
  expect_length(density, 4)
  expect_equal(result$n_rows, 2)
})

test_that("Check bayes_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(bayes_mcmc("faulty_model"), "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  expect_error(bayes_mcmc(list(a = 1)), "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  expect_error(bayes_mcmc(faulty_model), "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
})

test_that("{shinytest2} recording: e2e_bayes_mcmc", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_mcmc", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_mcmc")
  app$click("bayes_mcmc-run")
  app$wait_for_value(input = "bayes_mcmc-all-complete")
  app$wait_for_value(input = "bayes_mcmc-sub-complete")

  gelman_all <- app$get_value(output = "bayes_mcmc-all-gelman")
  gelman_sub <- app$get_value(output = "bayes_mcmc-sub-gelman")
  trace_all <- app$get_value(output = "bayes_mcmc-all-trace")
  trace_sub <- app$get_value(output = "bayes_mcmc-sub-trace")
  density_all <- app$get_value(output = "bayes_mcmc-all-density")
  density_sub <- app$get_value(output = "bayes_mcmc-sub-density")

  expect_equal(substr(gelman_all$src, 1, 10), "data:image")
  expect_equal(substr(gelman_sub$src, 1, 10), "data:image")
  expect_equal(substr(trace_all$src, 1, 10), "data:image")
  expect_equal(substr(trace_sub$src, 1, 10), "data:image")
  expect_equal(substr(density_all$src, 1, 10), "data:image")
  expect_equal(substr(density_sub$src, 1, 10), "data:image")

  app$stop()
})

