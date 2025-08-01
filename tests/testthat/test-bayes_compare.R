test_that("Check bayes_compare function works as expected", {
  result <- bayes_compare(fitted_bayes_model)
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
})

test_that("Check bayes_mcmc function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(bayes_compare("faulty_model"), "model must be an object created by bayes_model")
  expect_error(bayes_compare(list(a = 1)), "model must be an object created by bayes_model")
  expect_error(bayes_compare(faulty_model), "model must be an object created by bayes_model")
})

test_that("{shinytest2} recording: e2e_bayes_compare", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_compare", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")

  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_compare")
  app$click("bayes_compare-run")
  app$wait_for_value(output = "bayes_compare-all-table")
  app$wait_for_value(output = "bayes_compare-sub-table")

  table_all <- app$get_value(output = "bayes_compare-all-table")
  table_sub <- app$get_value(output = "bayes_compare-sub-table")

  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")

  # Test number of rows (including header)
  expect_equal(stringr::str_count(table_all, "<tr"), 7)
  expect_equal(stringr::str_count(table_sub, "<tr"), 5)

  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(table_all, "<tr>.+?</tr>"), "<th"), 6)
  expect_equal(stringr::str_count(stringr::str_extract(table_sub, "<tr>.+?</tr>"), "<th"), 4)

  table_all_dl <- app$get_download("bayes_compare-all-download")
  df <- read.csv(table_all_dl)
  # extra column which is hidden in app, header becomes names
  expect_equal(nrow(df), 6)
  expect_equal(ncol(df), 7)

  table_sub_dl <- app$get_download("bayes_compare-sub-download")
  df <- read.csv(table_sub_dl)
  expect_equal(nrow(df), 4)
  expect_equal(ncol(df), 5)

  app$stop()
})

