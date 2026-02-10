
test_that("Check covariate_ranking function works as expected", {
  result <- covariate_ranking(fitted_covariate_model, configured_data_con)

  expect_is(result, "list")
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(result)))
  expect_is(result$SUCRA, "data.frame")
  expect_is(result$Colour, "data.frame")
  expect_is(result$Cumulative, "data.frame")
  expect_is(result$Probabilities, "data.frame")
  expect_is(result$BUGSnetData, "BUGSnetData")

  table_result <- ranking_table(result)
  expect_is(table_result, "data.frame")
  expect_equal(table_result$Treatment[1], "Gabapentinoids")
  expect_equal(nrow(table_result), 4)
  expect_equal(ncol(table_result), 6)

  litmus_result <- LitmusRankOGram(result)
  expect_match(litmus_result, "<svg")

  sucra_result <- RadialSUCRA(result)
  expect_match(sucra_result, "<svg")
})

test_that("covariate_ranking produces errors for incorrect data types", {

  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(covariate_ranking(fitted_covariate_model, "not_data"), "configured_data must be of class configured_data")

  expect_error(covariate_ranking(faulty_model, configured_data_con), "model must be an object created by baseline_model")
  expect_error(covariate_ranking("faulty_model", configured_data_con), "model must be an object created by baseline_model")
  expect_error(covariate_ranking(list(a = 1), configured_data_con), "model must be an object created by baseline_model")

})

test_that("{shinytest2} recording: e2e_covariate_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_ranking")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_ranking")
  app$click("covariate_ranking-run")

  app$wait_for_value(output = "covariate_ranking-all-forest")

  common <- app$get_value(export = "common")
  expect_is(common$covariate_ranking, "list")
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(common$covariate_ranking)))

  forest_all <- app$get_value(output = "covariate_ranking-all-forest")
  expect_match(forest_all$html, "<svg")

  ranking_all <- app$get_value(output = "covariate_ranking-all-ranking")
  expect_match(ranking_all$html, "<svg")

  app$click("covariate_ranking-all-dropdown")
  ranking_table_all <- app$get_value(output = "covariate_ranking-all-ranking_table")
  expect_match(ranking_table_all, "<table")

  network_all <- app$get_value(output = "covariate_ranking-all-network")
  expect_match(network_all$html, "<svg")

  test_bayes_plot_downloads(app, "covariate_ranking", "_forest", FALSE)
  test_bayes_plot_downloads(app, "covariate_ranking", "_ranking_plot", FALSE)
  test_bayes_plot_downloads(app, "covariate_ranking", "_network", FALSE)

  ranking_table_dl_all <- app$get_download("covariate_ranking-all-download_ranking_table")

  df_all <- read.csv(ranking_table_dl_all)
  expect_equal(nrow(df_all), 6)
  expect_equal(ncol(df_all), 8)

  app$stop()

})

