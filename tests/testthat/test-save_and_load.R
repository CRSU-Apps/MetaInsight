test_that("The app can be saved after an analysis and the data restored", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$set_inputs(summarySel = "summary_char")
  app$set_inputs(main = "Results")
  app$set_inputs(summarySel = "summary_study")
  app$set_inputs(summarySel = "summary_network")
  app$set_inputs(main = "Save")

  save_file <- app$get_download("core_save-save_session")
  expect_gt(file.info(save_file)$size, 1000)
  common <- readRDS(save_file)

  # check data is in the save file (non-exhaustive)
  expect_s3_class(common$bugsnet_all, "data.frame")
  expect_s3_class(common$bugsnet_sub, "data.frame")
  expect_type(common$freq_all, "list")
  expect_type(common$freq_sub, "list")

  # this is important as this is the object used to reload module outputs on loading
  used_modules <- c("setup_load", "setup_define", "summary_exclusions", "summary_char", "summary_study", "summary_network")
  expect_true(all(used_modules %in% names(common$meta)))

  app$stop()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-load_session" = save_file)
  app$click("core_load-goLoad_session")
  common_restored <- app$get_value(export = "common")

  # check that data has been reloaded into common
  expect_s3_class(common_restored$bugsnet_all, "data.frame")
  expect_s3_class(common_restored$bugsnet_sub, "data.frame")
  expect_type(common_restored$freq_all, "list")
  expect_type(common_restored$freq_sub, "list")

  # check that input values have been restored
  restored_inputs <- app$get_values()
  expect_equal(restored_inputs$input[["summary_exclude-exclusions"]], c("Study01", "Study25"))

  app$stop()

})
