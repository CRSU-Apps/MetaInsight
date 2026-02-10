test_that("Check bayes_ranking function works as expected", {
  result <- bayes_ranking(fitted_bayes_model, configured_data_con)

  expect_is(result, "list")
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(result)))
  expect_is(result$SUCRA, "data.frame")
  expect_is(result$Colour, "data.frame")
  expect_is(result$Cumulative, "data.frame")
  expect_is(result$Probabilities, "data.frame")
  expect_is(result$BUGSnetData, "BUGSnetData")
  expect_equal(result$SUCRA$Treatment[1], "Gabapentinoids")

  table_result <- ranking_table(result)
  expect_is(table_result, "data.frame")
  expect_equal(nrow(table_result), 4)
  expect_equal(ncol(table_result), 6)

  litmus_result <- LitmusRankOGram(result)
  expect_match(litmus_result, "<svg")

  sucra_result <- RadialSUCRA(result)
  expect_match(sucra_result, "<svg")

})

test_that("bayes_ranking produces errors for incorrect data types", {

  faulty_model <- list(mtcRelEffects = 1:4)

  expect_error(bayes_ranking(faulty_model, configured_data_con), "model must be an object created by baseline_model")
  expect_error(bayes_ranking("faulty_model", configured_data_con), "model must be an object created by baseline_model")
  expect_error(bayes_ranking(list(a = 1), configured_data_con), "model must be an object created by baseline_model")
  expect_error(bayes_ranking("not_data", fitted_bayes_model), "configured_data must be of class configured_data")
})

test_that("{shinytest2} recording: e2e_bayes_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_ranking")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_ranking")
  app$click("bayes_ranking-run")

  app$wait_for_value(output = "bayes_ranking-all-forest")
  app$wait_for_value(output = "bayes_ranking-sub-forest")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_rank_all, "list")
  expect_is(common$bayes_rank_sub, "list")
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(common$bayes_rank_all)))
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(common$bayes_rank_sub)))

  forest_all <- app$get_value(output = "bayes_ranking-all-forest")
  forest_sub <- app$get_value(output = "bayes_ranking-sub-forest")
  expect_match(forest_all$html, "<svg")
  expect_match(forest_sub$html, "<svg")

  ranking_all <- app$get_value(output = "bayes_ranking-all-ranking")
  ranking_sub <- app$get_value(output = "bayes_ranking-sub-ranking")
  expect_match(ranking_all$html, "<svg")
  expect_match(ranking_sub$html, "<svg")

  app$click("bayes_ranking-all-dropdown")
  ranking_table_all <- app$get_value(output = "bayes_ranking-all-ranking_table")
  app$click("bayes_ranking-sub-dropdown")
  ranking_table_sub <- app$get_value(output = "bayes_ranking-sub-ranking_table")
  expect_match(ranking_table_all, "<table")
  expect_match(ranking_table_sub, "<table")

  network_all <- app$get_value(output = "bayes_ranking-all-network")
  network_sub <- app$get_value(output = "bayes_ranking-sub-network")
  expect_match(network_all$html, "<svg")
  expect_match(network_sub$html, "<svg")

  test_bayes_plot_downloads(app, "bayes_ranking", "_forest")
  test_bayes_plot_downloads(app, "bayes_ranking", "_ranking_plot")
  test_bayes_plot_downloads(app, "bayes_ranking", "_network")

  ranking_table_dl_all <- app$get_download("bayes_ranking-all-download_ranking_table")
  ranking_table_dl_sub <- app$get_download("bayes_ranking-sub-download_ranking_table")

  df_all <- read.csv(ranking_table_dl_all)
  expect_equal(nrow(df_all), 6)
  expect_equal(ncol(df_all), 8)

  df_sub <- read.csv(ranking_table_dl_sub)
  expect_equal(nrow(df_sub), 4)
  expect_equal(ncol(df_sub), 6)

  app$stop()
})

