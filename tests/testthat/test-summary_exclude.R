no_cv <- defined_data_con$non_covariate_data_all
t_df <- defined_data_con$treatment_df

no_cv_bin <- defined_data_bin$non_covariate_data_all
t_df_bin <- defined_data_bin$treatment_df

test_that("summary_exclude produces errors for incorrect data types", {
  expect_error(summary_exclude("not_a_dataframe", t_df, "Placebo", "Continuous", "MD", "random", c("Study01")), "non_covariate_data must be of class data.frame")
  expect_error(summary_exclude(no_cv, "not_a_dataframe", "Placebo", "Continuous", "MD", "random", c("Study01")), "treatment_df must be of class data.frame")
  expect_error(summary_exclude(no_cv, t_df, 123, "Continuous", "MD", "random", c("Study01")), "reference_treatment must be of class character")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", 123, "MD", "random", c("Study01")), "outcome must be of class character")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", "Continuous", 123, "random", c("Study01")), "outcome_measure must be of class character")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", "Continuous", "MD", 123, c("Study01")), "model_type must be of class character")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", "Continuous", "MD", "random", 123), "exclusions must be of class character")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", "invalid_outcome", "MD", "random", c("Study01")), "outcome must be either Binary or Continuous")
  expect_error(summary_exclude(no_cv, t_df, "Placebo", "Continuous", "invalid_measure", "random", c("Study01")), "outcome_measure must be either MD or SMD")
  expect_error(summary_exclude(no_cv_bin, t_df_bin, "Placebo", "Binary", "invalid_measure", "random", c("Study01")), "outcome_measure must be either OR, RR or RD")
})

test_that("summary_exclude produces data of the correct type", {
  result <- summary_exclude(no_cv, t_df, "Placebo", "Continuous", "MD", "random", c("Study01"))

  expected_items <- c("freq_sub", "bugsnet_sub")
  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))
  expect_s3_class(result$bugsnet_sub, "data.frame")
  expect_type(result$freq_sub, "list")
})


test_that("summary_exclude removes the correct studies", {
  result <- summary_exclude(no_cv, t_df, "Placebo", "Continuous", "MD", "random", c("Study01", "Study25"))
  n_studies_all <- length(unique(no_cv$Study))
  n_studies_sub_bugs <- length(unique(result$bugsnet_sub$Study))
  n_studies_sub_freq <- length(unique(result$freq_sub$d0$Study))

  expect_false("Study01" %in% result$bugsnet_sub$Study)
  expect_false("Study25" %in% result$bugsnet_sub$Study)
  expect_true("Study02" %in% result$bugsnet_sub$Study)
  expect_equal(n_studies_all - 2, n_studies_sub_bugs)

  expect_false("Study01" %in% result$freq_sub$d0$Study)
  expect_false("Study25" %in% result$freq_sub$d0$Study)
  expect_true("Study02" %in% result$freq_sub$d0$Study)
  expect_equal(n_studies_all - 2, n_studies_sub_freq)

})

test_that("summary exclude loads data into common correctly", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_setup_load")
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  common <- app$get_value(export = "common")

  expect_s3_class(common$bugsnet_sub, "data.frame")
  expect_type(common$freq_sub, "list")

  n_studies_all <- length(unique(no_cv$Study))
  n_studies_sub_bugs <- length(unique(common$bugsnet_sub$Study))
  n_studies_sub_freq <- length(unique(common$freq_sub$d0$Study))

  expect_false("Study01" %in% common$bugsnet_sub$Study)
  expect_false("Study25" %in% common$bugsnet_sub$Study)
  expect_true("Study02" %in% common$bugsnet_sub$Study)
  expect_equal(n_studies_all - 2, n_studies_sub_bugs)

  expect_false("Study01" %in% common$freq_sub$d0$Study)
  expect_false("Study25" %in% common$freq_sub$d0$Study)
  expect_true("Study02" %in% common$freq_sub$d0$Study)
  expect_equal(n_studies_all - 2, n_studies_sub_freq)

  app$stop()
})


