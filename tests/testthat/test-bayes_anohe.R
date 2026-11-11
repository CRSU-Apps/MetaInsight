test_that("Check bayes_anohe function works as expected", {
  result <- bayes_anohe(fitted_bayes_model)
  expect_is(result, "data.frame")
  expect_equal(ncol(result), 5)
})

test_that("Check bayes_anohe function produces errors as expected", {
  faulty_model <- list(mtcResults = 1:4)
  expect_error(bayes_compare("faulty_model"), "model must be an object created by bayes_model")
  expect_error(bayes_compare(list(a = 1)), "model must be an object created by bayes_model")
  expect_error(bayes_compare(faulty_model), "model must be an object created by bayes_model")
})


test_that("{shinytest2} recording: e2e_bayes_anohe", {
  skip_if(skip_shinytest2)
  
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_bayes_anohe", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")
  
  app$set_inputs(tabs = "bayes")
  app$set_inputs(bayesSel = "bayes_anohe")
  app$click("bayes_anohe-run")
  app$wait_for_value(input = "bayes_anohe-all-complete")
  app$wait_for_value(input = "bayes_anohe-sub-complete")
  
  table_all <- app$get_value(output = "bayes_anohe-all-table")
  table_sub <- app$get_value(output = "bayes_anohe-sub-table")
  
  expect_match(table_all, "<table")
  expect_match(table_sub, "<table")
  
  # Test number of columns
  expect_equal(stringr::str_count(stringr::str_extract(table_all, "<tr>.+?</tr>"), "<th"), 6)
  expect_equal(stringr::str_count(stringr::str_extract(table_sub, "<tr>.+?</tr>"), "<th"), 6)

  table_all_dl <- app$get_download("bayes_anohe-all-download")
  df <- read.csv(table_all_dl)
  expect_equal(ncol(df), 5)

  table_sub_dl <- app$get_download("bayes_anohe-sub-download")
  df <- read.csv(table_sub_dl)
  expect_equal(ncol(df), 5)
  
  app$stop()
})
