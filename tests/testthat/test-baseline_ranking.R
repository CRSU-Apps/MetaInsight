test_that("Check baseline_ranking function works as expected", {
  result <- baseline_ranking(defined_data_con$main_connected_data,
                              loaded_data_con$treatment_df,
                              fitted_baseline_model,
                              "good")

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
  expect_is(litmus_result, "ggplot")

  sucra_result <- RadialSUCRA(result)
  expect_is(sucra_result, "list")
  expect_is(sucra_result$Original, "ggplot")
  expect_is(sucra_result$Alternative, "ggplot")

})

test_that("{shinytest2} recording: e2e_baseline_ranking", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_ranking")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")
  app$set_inputs(baselineSel = "baseline_ranking")
  app$click("baseline_ranking-run")

  app$wait_for_value(output = "baseline_ranking-all-forest")

  common <- app$get_value(export = "common")
  expect_is(common$bayes_ranking, "list")
  expect_true(all(c("SUCRA", "Colour", "Cumulative", "Probabilities", "BUGSnetData") %in% names(common$bayes_ranking)))

  forest_all <- app$get_value(output = "baseline_ranking-all-forest")
  expect_match(forest_all$html, "<svg")

  ranking_all <- app$get_value(output = "baseline_ranking-all-ranking")
  expect_equal(substr(ranking_all$src, 1, 10), "data:image")

  app$click("baseline_ranking-all-dropdown")
  ranking_table_all <- app$get_value(output = "baseline_ranking-all-ranking_table")
  expect_match(ranking_table_all, "<table")

  network_all <- app$get_value(output = "baseline_ranking-all-network")
  expect_match(network_all$html, "<svg")

  test_bayes_plot_downloads(app, "baseline_ranking", "_forest", FALSE)
  test_bayes_plot_downloads(app, "baseline_ranking", "_network", FALSE)

  # only as a png at present
  ranking_png_all <- app$get_download("baseline_ranking-all-download_ranking_plot")
  expect_gt(file.info(ranking_png_all)$size, 1000)
  expect_true(validate_plot(ranking_png_all, "png"))
  unlink(ranking_png_all)

  ranking_table_dl_all <- app$get_download("baseline_ranking-all-download_ranking_table")

  df_all <- read.csv(ranking_table_dl_all)
  expect_equal(nrow(df_all), 6)
  expect_equal(ncol(df_all), 8)

  app$stop()

})

