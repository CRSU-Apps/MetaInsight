test_that("Check bayes_ranking function works as expected", {
  result <- bayes_ranking(defined_data_con$main_connected_data,
                          loaded_data_con$treatment_df,
                          fitted_bayes_model,
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
  expect_equal(substr(ranking_all$src, 1, 10), "data:image")
  expect_equal(substr(ranking_sub$src, 1, 10), "data:image")

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
  test_bayes_plot_downloads(app, "bayes_ranking", "_network")

  # only as a png at present
  ranking_png_all <- app$get_download("bayes_ranking-all-download_ranking_plot")
  ranking_png_sub <- app$get_download("bayes_ranking-sub-download_ranking_plot")
  expect_gt(file.info(ranking_png_all)$size, 1000)
  expect_gt(file.info(ranking_png_sub)$size, 1000)
  expect_true(validate_plot(ranking_png_all, "png"))
  expect_true(validate_plot(ranking_png_sub, "png"))
  unlink(c(ranking_png_all, ranking_png_sub))

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

