test_that("Check bayes_mcmc function works as expected", {
  result <- bayes_mcmc(fitted_bayes_model)
  expect_type(result, "list")
  expect_true(all(c("gelman_plots", "trace_plots", "density_plots", "n_rows") %in% names(result)))
  expect_is(result$gelman_plots[[1]], "function")
  expect_is(result$trace_plots[[1]], "ggplot")
  expect_is(result$density_plots[[1]], "ggplot")
  expect_is(result$n_rows, "numeric")

  expect_length(result$gelman_plots, 4)
  expect_length(result$trace_plots, 4)
  expect_length(result$density_plots, 4)
  expect_equal(result$n_rows, 2)
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

