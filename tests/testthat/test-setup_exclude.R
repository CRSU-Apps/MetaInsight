test_that("setup_exclude produces errors for incorrect data types", {
  expect_error(setup_exclude("not_configured", c("Leo")), "configured_data must be of class configured_data")
  expect_error(setup_exclude(configured_data_con, 123), "exclusions must be of class character")

  expect_error(setup_exclude(configured_data_con, c("Study1000")), "exclusions must in the present in the loaded data")
})

test_that("setup_exclude produces data of the correct type", {
  result <- setup_exclude(configured_data_con, c("Leo"))

  expected_items <- c("treatments", "connected_data", "covariate", "freq", "outcome",
                       "outcome_measure", "effects", "ranking_option", "seed")

  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_s3_class(result$connected_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_type(result$covariate$column, "character")
  expect_type(result$covariate$name, "character")
  expect_type(result$covariate$type, "character")
  expect_type(result$freq, "list")
  expect_type(result$outcome, "character")
  expect_type(result$outcome_measure, "character")
  expect_type(result$effects, "character")
  expect_type(result$ranking_option, "character")
  expect_type(result$seed, "double")

})

test_that("setup_exclude removes the correct studies", {
  result <- setup_exclude(configured_data_con, c("Leo", "Constantine"))
  n_studies_all <- length(unique(configured_data_con$connected_data$Study))
  n_studies_freq <- length(unique(result$freq$d0$Study))

  expect_false("Leo" %in% result$freq$d0$Study)
  expect_false("Constantine" %in% result$freq$d0$Study)
  expect_true("Justinian" %in% result$freq$d0$Study)
  expect_equal(n_studies_all - 2, n_studies_freq)

})

test_that("setup_exclude loads data into common correctly", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
  app$set_inputs(tabs = "setup")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs("setup_exclude-exclusions" = c("Leo", "Constantine"))
  app$wait_for_value(input = "setup_exclude-complete", ignore = list(NULL, "", "initial"))

  common <- app$get_value(export = "common")

  expect_type(common$subsetted_data$freq, "list")
  expect_type(common$subsetted_data$reference_treatment, "character")

  n_studies_all <- length(unique(common$configured_data$freq$d0$Study))
  n_studies_sub_freq <- length(unique(common$subsetted_data$freq$d0$Study))

  expect_false("Leo" %in% common$subsetted_data$freq$d0$Study)
  expect_false("Constantine" %in% common$subsetted_data$freq$d0$Study)
  expect_true("Justinian" %in% common$subsetted_data$freq$d0$Study)
  expect_equal(n_studies_all - 2, n_studies_sub_freq)

  app$stop()
})

test_that("setup_exclude launches a note when reference_treatment_sub changes", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
  app$set_inputs(tabs = "setup")
  app$click("setup_load-run")
  app$set_inputs("setupSel" = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  # this uses unformatted treatments initially
  app$set_inputs("setup_configure-reference_treatment" = "the Little")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")
  app$set_inputs("setup_exclude-exclusions" = "Leo")
  app$wait_for_value(input = "setup_exclude-complete", ignore = list(NULL, "", "initial"))

  common <- app$get_value(export = "common")
  expect_equal(common$subsetted_data$reference_treatment, "the_Great")
  expect_false(common$subsetted_data$reference_treatment == common$configured_data$reference_treatment)

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*has been changed to the_Great*", logger))

  app$stop()
})

