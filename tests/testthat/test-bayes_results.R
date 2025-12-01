test_that("Check bayes_results function works as expected", {
  result <- bayes_results(fitted_bayes_model)
  expect_is(result, "html")
  expect_equal(stringr::str_count(result, "<br"), 5)
})

test_that("{shinytest2} recording: e2e_bayes_results", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_results")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_results")
  app$click("bayes_results-run")
  app$wait_for_value(output = "bayes_results-all-statistics")
  app$wait_for_value(output = "bayes_results-sub-statistics")

  stats_all <- app$get_value(output = "bayes_results-all-statistics")
  stats_sub <- app$get_value(output = "bayes_results-sub-statistics")
  expect_match(stats_all, "<table")
  expect_match(stats_sub, "<table")
  # Test number of rows (including header)
  expect_equal(stringr::str_count(stats_all, "<tr"), 7)
  expect_equal(stringr::str_count(stats_sub, "<tr"), 5)
  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(stats_all, "<tr>.+?</tr>"), "<th"), 5)
  expect_equal(stringr::str_count(stringr::str_extract(stats_sub, "<tr>.+?</tr>"), "<th"), 5)

  quant_all <- app$get_value(output = "bayes_results-all-quantiles")
  quant_sub <- app$get_value(output = "bayes_results-sub-quantiles")
  expect_match(quant_all, "<table")
  expect_match(quant_sub, "<table")
  # Test number of rows (including header)
  expect_equal(stringr::str_count(quant_all, "<tr"), 7)
  expect_equal(stringr::str_count(quant_sub, "<tr"), 5)
  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(quant_all, "<tr>.+?</tr>"), "<th"), 6)
  expect_equal(stringr::str_count(stringr::str_extract(quant_sub, "<tr>.+?</tr>"), "<th"), 6)

  text_all <- app$get_value(output = "bayes_results-all-text")
  text_sub <- app$get_value(output = "bayes_results-sub-text")
  expect_match(text_all$html, "Iterations")
  expect_match(text_sub$html, "Iterations")

  app$stop()
})

