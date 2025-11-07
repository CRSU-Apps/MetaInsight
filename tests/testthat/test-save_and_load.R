# my PC has a pecularity where the upload of temporary files fails
if (Sys.getenv("GITHUB_ACTIONS") == "true"){
  save_file <- tempfile(fileext = ".rds")
} else {
  save_file <- "~/save_file.rds"
}

test_that("The app can be saved after an analysis and the data restored", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)
  app$set_inputs(tabs = "setup")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")

  app$get_download("core_save-save_session", filename = save_file)
  expect_gt(file.info(save_file)$size, 1000)
  common_saved <- readRDS(save_file)

  # check data is in the save file (non-exhaustive)
  expect_s3_class(common_saved$bugsnet_all, "data.frame")
  expect_s3_class(common_saved$bugsnet_sub, "data.frame")
  expect_type(common_saved$freq_all, "list")
  expect_type(common_saved$freq_sub, "list")

  # this is important as this is the object used to reload module outputs on loading
  used_modules <- c("setup_load", "setup_configure", "setup_exclude")
  expect_true(all(used_modules %in% names(common_saved$meta)))

  app$stop()

  app_load <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  expect_true(file.exists(save_file))
  app_load$set_inputs(tabs = "setup")
  app_load$set_inputs(setupSel = "setup_load")
  app_load$upload_file("setup_reload-load_session" = save_file)
  app_load$click("setup_reload-goLoad_session")

  # check that data has been reloaded into common
  common_restored <- app_load$get_value(export = "common")
  expect_s3_class(common_restored$bugsnet_all, "data.frame")
  expect_s3_class(common_restored$bugsnet_sub, "data.frame")
  expect_type(common_restored$freq_all, "list")
  expect_type(common_restored$freq_sub, "list")

  # check that input values have been restored
  # should be expanded upon...
  app_load$wait_for_value(input = "setup_exclude-exclusions")
  restored_input <- app_load$get_value(input = "setup_exclude-exclusions")
  expect_equal(restored_input, c("Study01", "Study25"))

  app_load$stop()

})
