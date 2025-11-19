test_that("Check baseline_mcmc function works as expected", {
  result <- baseline_mcmc(fitted_baseline_model)
  expect_type(result, "list")
  expect_true(all(c("parameters", "gelman_data", "n_cols", "n_rows", "n_rows_rmd") %in% names(result)))

  expect_is(result$parameters, "character")
  expect_is(result$gelman_data, "list")
  expect_is(result$n_cols, "numeric")
  expect_is(result$n_rows, "numeric")
  expect_is(result$n_rows_rmd, "numeric")

  gelman <- gelman_plots(result$gelman_data, result$parameters)
  trace <- trace_plots(fitted_baseline_model$mtcResults, result$parameters)
  density <- density_plots(fitted_baseline_model$mtcResults, result$parameters)

  expect_is(gelman[[1]], "ggplot")
  expect_is(trace[[1]], "ggplot")
  expect_is(density[[1]], "ggplot")

  expect_length(gelman, 7)
  expect_length(trace, 7)
  expect_length(density, 7)
  expect_equal(result$n_cols, 4)
})

test_that("Check baseline_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(baseline_mcmc("faulty_model"), "model must be an object created by baseline_model")
  expect_error(baseline_mcmc(list(a = 1)), "model must be an object created by baseline_model")
  expect_error(baseline_mcmc(faulty_model), "model must be an object created by baseline_model")
})

test_that("{shinytest2} recording: e2e_baseline_mcmc", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_baseline_mcmc", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_mcmc")
  app$click("baseline_mcmc-run")
  app$wait_for_value(input = "baseline_mcmc-complete")

  gelman_all <- app$get_value(output = "baseline_mcmc-all-gelman")
  trace_all <- app$get_value(output = "baseline_mcmc-all-trace")
  density_all <- app$get_value(output = "baseline_mcmc-all-density")

  expect_equal(substr(gelman_all$src, 1, 10), "data:image")
  expect_equal(substr(trace_all$src, 1, 10), "data:image")
  expect_equal(substr(density_all$src, 1, 10), "data:image")

  app$stop()
})

