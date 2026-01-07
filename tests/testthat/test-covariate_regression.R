test_that("CalculateDirectness() gathers covariate values for studies", {
  data <- read.csv(file.path(test_data_dir, "Test_directness.csv"))
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- ReplaceTreatmentIds(data, treatment_ids)

  contributions <- CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome = "Binary",
    outcome_measure = "OR",
    effects_type = "random"
  )

  studies <- unique(data$Study)

  expected_covariate_values <- unique(data[["covar.age"]])
  names(expected_covariate_values) <- studies

  covariate_values <- contributions$covariate_value
  names(covariate_values) <- studies
  expect_equal(
    !!covariate_values,
    !!expected_covariate_values
  )
})

test_that("CalculateDirectness() correctly calculates directness", {
  data <- read.csv(file.path(test_data_dir, "Test_directness.csv"))
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- ReplaceTreatmentIds(data, treatment_ids)

  contributions <- CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome = "Binary",
    outcome_measure = "OR",
    effects_type = "random"
  )

  studies <- unique(data$Study)

  expected_is_direct <- matrix(
    data = c(
      TRUE, FALSE, FALSE,
      TRUE, TRUE, FALSE,
      FALSE, TRUE, FALSE,
      FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )

  row.names(expected_is_direct) <- studies
  colnames(expected_is_direct) <- treatment_ids$Label[-1]

  expected_is_indirect <- matrix(
    data = c(
      FALSE, TRUE, FALSE,
      TRUE, TRUE, FALSE,
      TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE,
      TRUE, TRUE, FALSE
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )

  row.names(expected_is_indirect) <- studies
  colnames(expected_is_indirect) <- treatment_ids$Label[-1]

  expect_equal(
    !!contributions$is_direct,
    !!expected_is_direct
  )
  expect_equal(
    !!contributions$is_indirect,
    !!expected_is_indirect
  )
})

test_that("CalculateContributions() gathers relative treatment effects for treatments and studies", {
  data <- read.csv(file.path(test_data_dir, "Test_directness.csv"))
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- metainsight:::ReplaceTreatmentIds(data, treatment_ids)

  contributions <- metainsight:::CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome = "Binary",
    outcome_measure = "OR",
    effects_type = "random"
  )

  studies <- unique(data$Study)

  #Create empty expected relative effects matrix
  expected_relative_effects <- matrix(
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  #Populate expected relative effects
  expected_relative_effects[1, 1] <- qlogis(85 / 114) - qlogis(39 / 113)
  expected_relative_effects[2, 1] <- qlogis(59 / 66) - qlogis(36 / 66)
  expected_relative_effects[2, 2] <- qlogis(53 / 66) - qlogis(36 / 66)
  expected_relative_effects[3, 2] <- qlogis(78 / 89) - qlogis(36 / 88)
  expected_relative_effects[4, 3] <- qlogis(126 / 134) - qlogis(76 / 132)

  row.names(expected_relative_effects) <- studies
  colnames(expected_relative_effects) <- treatment_ids$Label[-1]

  expect_equal(
    !!contributions$relative_effect,
    !!expected_relative_effects
  )
})

test_that("Check covariate_regression function works as expected", {
  result <- covariate_regression(fitted_covariate_model,
                                defined_data_con$main_connected_data,
                                "covar.age",
                                defined_data_con$treatment_df,
                                "Continuous",
                                "MD",
                                "random")

  expect_is(result, "list")
  expect_true(all(c("directness",
                    "credible_regions") %in% names(result)))
  expect_is(result$directness, "list")
  expect_is(result$credible_regions, "list")

  expect_true(all(c("is_direct",
                    "is_indirect",
                    "relative_effect",
                    "covariate_value") %in% names(result$directness)))

  expect_true(all(c("regions",
                    "intervals") %in% names(result$credible_regions)))

  plot_result <- metaregression_plot(fitted_covariate_model,
                                     defined_data_con$treatment_df,
                                     "MD",
                                     c("Gabapentinoids", "Ketamine"),
                                     result$directness,
                                     result$credible_regions)

  expect_match(plot_result, "<svg")

})

test_that("{shinytest2} recording: e2e_covariate_regression", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), name = "e2e_covariate_regression", timeout = 30000)
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "covariate")
  app$set_inputs(covariateSel = "covariate_regression")
  # needed to avoid error
  app$set_inputs("covariate_regression-covariate-credible" = FALSE)
  app$click("covariate_regression-covariate-run")
  app$wait_for_value(input = "covariate_regression-complete")

  common <- app$get_value(export = "common")
  result <- common$covariate_regression

  expect_is(result, "list")
  expect_true(all(c("directness",
                    "credible_regions") %in% names(result)))
  expect_is(result$directness, "list")
  expect_is(result$credible_regions, "list")

  expect_true(all(c("is_direct",
                    "is_indirect",
                    "relative_effect",
                    "covariate_value") %in% names(result$directness)))

  expect_true(all(c("regions",
                    "intervals") %in% names(result$credible_regions)))

  app$wait_for_value(output = "covariate_regression-covariate-plot")
  plot <- app$get_value(output = "covariate_regression-covariate-plot")
  expect_match(plot$html, "<svg")

  test_plot_downloads(app, "covariate_regression-covariate", FALSE)

  app$stop()
})

