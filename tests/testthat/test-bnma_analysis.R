test_that("FormatForBnma() gives correct data for wide binary", {

  # process data as would be in app
  data <- read.csv(file.path(test_data_dir, "Binary_wide_continuous_cov.csv"))
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  bnma_data <- FormatForBnma(connected_data=data,
                             treatment_df=wrangled_treatment_list,
                             outcome="binary",
                             reference_treatment="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = c(30, 31, 32, 34, 33, 35, 36, 37),
    N = c(100, 101, 102, 104, 103, 105, 106, 107))

  #The expected order is the reference first, followed by the rest in the order they appear in the long version of the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(WideToLong(data, outcome="binary")$T)], first_item = "the_Little")

  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for long binary", {

  data <- read.csv(file.path(test_data_dir, "Binary_long_continuous_cov.csv"))
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  bnma_data <- FormatForBnma(connected_data=data,
                             treatment_df=wrangled_treatment_list,
                             outcome="binary",
                             reference_treatment="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = as.integer(c(30, 31, 32, 34, 33, 35, 36, 37)),
    N = as.integer(c(100, 101, 102, 104, 103, 105, 106,107)))
  row.names(expected_ArmLevel) <- as.integer(c(1, 2, 3, 5, 4, 6, 7, 8))

  #The expected order is the reference first, followed by the rest in the order they appear in the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(data$T)], first_item = "the_Little")

  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for wide continuous", {

  data <- read.csv(file.path(test_data_dir, "Cont_wide_continuous_cov.csv"))
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  bnma_data <- FormatForBnma(connected_data=data,
                             treatment_df=wrangled_treatment_list,
                             outcome="continuous",
                             reference_treatment="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = c(-1, -1.1, -1.2, -1.4, -1.3, -1.5, -1.6, -1.7),
    SD = c(11.1, 12.2, 13.3, 15.5, 14.4, 16.6, 17.7, 18.8),
    N = c(30, 31, 32, 34, 33, 35, 36, 37))

  #The expected order is the reference first, followed by the rest in the order they appear in the long version of the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(WideToLong(data, outcome="binary")$T)], first_item = "the_Little")

  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for long continuous", {

  data <- read.csv(file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  bnma_data <- FormatForBnma(connected_data=data,
                             treatment_df=wrangled_treatment_list,
                             outcome="continuous",
                             reference_treatment="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = c(-1, -1.1, -1.2, -1.4, -1.3, -1.5, -1.6, -1.7),
    SD = c(11.1, 12.2, 13.3, 15.5, 14.4, 16.6, 17.7, 18.8),
    N = c(30, 31, 32, 34, 33, 35, 36, 37))
  row.names(expected_ArmLevel) <- as.integer(c(1, 2, 3, 5, 4, 6, 7, 8))

  #The expected order is the reference first, followed by the rest in the order they appear in the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(data$T)], first_item = "the_Little")

  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("BaselineRiskNetwork() assigns model type, covariate type and reference treatment correctly", {

  data <- list(ArmLevel = data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = 30:37,
    N = 100:107)
  )
  data$Treat.order <- VectorWithItemFirst(vector = unique(data$ArmLevel$Treat), first_item = "the_Great")

  bnma_network <- BaselineRiskNetwork(br_data = data,
                                      outcome = "binary",
                                      model_type = "random",
                                      cov_parameters = "unrelated")

  expect_equal(bnma_network$response, "binomial")
  expect_equal(bnma_network$type, "random")
  expect_equal(bnma_network$baseline, "independent")
  expect_equal(as.character(bnma_network$Treat.order[1]), "the_Great")
})



test_that("BaselineRiskNetwork() has correct model settings for Binary outcome", {

  data <- list(ArmLevel = data.frame(
      Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      Outcomes = 30:37,
      N = 100:107))
  data$Treat.order <- VectorWithItemFirst(vector = unique(data$ArmLevel$Treat), first_item = "the_Great")

  bnma_network <- BaselineRiskNetwork(br_data = data,
                                      outcome = "binary",
                                      model_type = "random",
                                      cov_parameters = "unrelated")

  expect_equal(bnma_network$response, "binomial")
})



test_that("BaselineRiskNetwork() has correct model settings for Continuous outcome", {

  data <- list(ArmLevel = data.frame(
      Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      Outcomes = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
      SD = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
      N = 30:37))
  data$Treat.order <- VectorWithItemFirst(vector = unique(data$ArmLevel$Treat), first_item = "the_Great")

  bnma_network <- BaselineRiskNetwork(br_data = data,
                                      outcome = "continuous",
                                      model_type = "fixed",
                                      cov_parameters = "exchangeable")

  expect_equal(bnma_network$response, "normal")
})



test_that("GetReferenceOutcome() returns the reference outcome when the outcome is binary", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(5, 7, 4, 5, 2, 6, 7, 3, 5),
                     N = 30:38)

  treatment_ids <- list(Number = 1:4, Label = c("Hydrogen", "Oxygen", "Sulphur", "Zinc"))
  outcome_type <- "binary"
  observed <- "Observed"

  expected_reference_outcome <- c(log(5 / (30 - 5)), NA, log(2 / (34 - 2)), NA)
  names(expected_reference_outcome) <- c("A", "B", "C", "D")

  expect_equal(GetReferenceOutcome(data, treatment_ids, outcome_type, observed), expected_reference_outcome)
})



test_that("GetReferenceOutcome() returns the reference outcome when the outcome is continuous", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     Mean = c(5, 7, 4, 5, 2, 6, 7, 3, 5),
                     N = 30:38,
                     SD = c(2, 3, 4, 2, 3, 4, 2, 3, 4))

  treatment_ids <- list(Number = 1:4, Label = c("Hydrogen", "Oxygen", "Sulphur", "Zinc"))
  outcome_type <- "continuous"
  observed <- "Observed"

  expected_reference_outcome <- c(5, NA, 2, NA)
  names(expected_reference_outcome) <- c("A", "B", "C", "D")

  expect_equal(GetReferenceOutcome(data, treatment_ids, outcome_type, observed), expected_reference_outcome)
})

test_that("1. BaselineRiskRegression() sets RNGs correctly;
           2. BaselineRiskRegression() gives reproducible output;
           3. BaselineRiskModelOutput() gives correct output;
           4. BnmaRelativeEffects() calculates relative effects;
           5. GetReferenceOutcome() obtains imputed outcomes;
           6. GetBnmaMcmcCharacteristics() returns correct MCMC data;
           7. GetBnmaPriors() returns correct prior distributions.", {

  data <- list(ArmLevel = data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    T = c(1, 2, 3, 4, 1, 5, 2, 6),
    Mean = c(-1, -2.1, -3.2, -4.3, -1.4, -5.5, -1.6, -7.7),
    SD = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    N = 30:37))
  data$Treat.order <- VectorWithItemFirst(vector = unique(data$ArmLevel$Treat), first_item = "the_Great")

  treatment_ids <- data.frame(Number = 1:6,
                              Label = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Butcher", "the_Slit_nosed"))

  result_1 <- BaselineRiskRegression(connected_data = data$ArmLevel,
                                     treatment_df = treatment_ids,
                                     outcome = "continuous",
                                     reference_treatment = "the_Great",
                                     model_type = "random",
                                     cov_parameters = "exchangeable",
                                     seed = 97531) |> suppress_jags_output()

  result_2 <- BaselineRiskRegression(connected_data = data$ArmLevel,
                                     treatment_df = treatment_ids,
                                     outcome = "continuous",
                                     reference_treatment = "the_Great",
                                     model_type = "random",
                                     cov_parameters = "exchangeable",
                                     seed = 97531) |> suppress_jags_output()

  #Unit test 1
  expect_equal(result_1$inits[[1]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[2]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[3]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[4]]$.RNG.name, "base::Wichmann-Hill")
  #-------------------------------------------------------------------

  #Unit test 2
  expect_equal(result_1$samples[1], result_2$samples[1])
  expect_equal(result_1$samples[2], result_2$samples[2])
  expect_equal(result_1$samples[3], result_2$samples[3])
  expect_equal(result_1$samples[4], result_2$samples[4])
  #-------------------------------------------------------------------

  model_output <- BaselineRiskModelOutput(connected_data = data$ArmLevel,
                                          treatment_df = treatment_ids,
                                          model = result_1,
                                          outcome_measure = "MD")

  summary_1 <- summary(result_1)

  expected_mtcResults <- result_1
  expected_covariate_value <- mean(c(-1, -1.4))
  expected_reference_treatment <- treatment_ids$Label[1]
  expected_comparator_names <- treatment_ids$Label[-1]
  expected_effects_type_text <- "random effect"
  expected_cov_value_sentence <- paste("Value for baseline risk set at", round(expected_covariate_value, 2), "")
  expected_slopes <- summary_1$summary.samples$quantiles[c("b_bl[2]", "b_bl[3]", "b_bl[4]", "b_bl[5]", "b_bl[6]"), "50%"]
  names(expected_slopes) <- expected_comparator_names
  expected_centred_intercepts <- summary_1$summary.samples$quantiles[c("d[2]", "d[3]", "d[4]", "d[5]", "d[6]"), "50%"]
  expected_intercepts <- expected_centred_intercepts - expected_slopes * expected_covariate_value
  names(expected_intercepts) <- expected_comparator_names
  expected_outcome <- "continuous"
  expected_outcome_measure <- "MD"
  expected_effects_type <- "random"
  expected_covariate_min <- c(-1, -1, -1.4, -1.4, NA)
  names(expected_covariate_min) <- expected_comparator_names
  expected_covariate_max <- c(-1, -1, -1.4, -1.4, NA)
  names(expected_covariate_max) <- expected_comparator_names
  expected_dic <- c(8.34347465810339, 7.33645461865518, 15.6799292767586, 9) |> as.data.frame()
  # getting different results on mac (below) and ubuntu
  # expected_dic <- c(8.33979715193098, 7.3344123200321, 15.6742094719631, 9) |> as.data.frame()
  rownames(expected_dic) <- c("Dbar", "pD", "DIC", "Data points")
  colnames(expected_dic) <- "BaselineRiskDicTable(model)"
  expected_summary <- summary_1
  names(expected_summary)[1] <- "summaries"
  expected_summary$measure <- "Mean Difference"

  expected_model_output <- list(
    mtcResults = expected_mtcResults,
    covariate_value = expected_covariate_value,
    reference_treatment = expected_reference_treatment,
    comparator_names = expected_comparator_names,
    a = expected_effects_type_text,
    cov_value_sentence = expected_cov_value_sentence,
    slopes = expected_slopes,
    intercepts = expected_intercepts,
    outcome = expected_outcome,
    outcome_measure = expected_outcome_measure,
    effects = expected_effects_type,
    covariate_min = expected_covariate_min,
    covariate_max = expected_covariate_max,
    dic = expected_dic,
    sumresults = expected_summary
  )

  #Unit test 3
  expect_equal(model_output, expected_model_output)
  #-------------------------------------------------------------------

  relative_effects <- BnmaRelativeEffects(model = result_1, covariate_value = 5)

  #For each parameter, combine the chains into one vector
  combined_samples <- list()
  for (i in 2:6) {
    combined_samples[[paste0("d", i)]] <- c(result_1$samples[[1]][, paste0("d[", i, "]")],
                                            result_1$samples[[2]][, paste0("d[", i, "]")],
                                            result_1$samples[[3]][, paste0("d[", i, "]")])
    combined_samples[[paste0("b_bl", i)]] <- c(result_1$samples[[1]][, paste0("b_bl[", i, "]")],
                                               result_1$samples[[2]][, paste0("b_bl[", i, "]")],
                                               result_1$samples[[3]][, paste0("b_bl[", i, "]")])
  }

  centred_covariate_value <- 5 - result_1$network$mx_bl

  #For each basic comparison, obtain the samples corresponding to the relative effect
  rel_eff_samples <- list()
  for (i in 2:6) {
    rel_eff_samples[[paste0("d", i)]] <-
      combined_samples[[paste0("d", i)]] + centred_covariate_value * combined_samples[[paste0("b_bl", i)]]
  }

  expected_relative_effects <- rbind(quantile(rel_eff_samples[["d2"]], probs = c(0.5, 0.025, 0.975)),
                                     quantile(rel_eff_samples[["d3"]], probs = c(0.5, 0.025, 0.975)),
                                     quantile(rel_eff_samples[["d4"]], probs = c(0.5, 0.025, 0.975)),
                                     quantile(rel_eff_samples[["d5"]], probs = c(0.5, 0.025, 0.975)),
                                     quantile(rel_eff_samples[["d6"]], probs = c(0.5, 0.025, 0.975)))
  rownames(expected_relative_effects) <- c("d[2]", "d[3]", "d[4]", "d[5]", "d[6]")

  #Unit test 4
  expect_equal(relative_effects, expected_relative_effects)
  #-------------------------------------------------------------------

  outcome_type <- "continuous"
  observed <- "Imputed"
  imputed_outcome_study_3 <- MCMCvis::MCMCsummary(object = result_1$samples, params = "Eta")["50%"]["Eta[3]", 1]
  expected_reference_outcome <- c(-1, -1.4, imputed_outcome_study_3)
  names(expected_reference_outcome) <- c("Constantine", "Leo", "Justinian")

  #Unit test 5
  expect_equal(GetReferenceOutcome(data$ArmLevel, treatment_ids, outcome_type, observed, result_1), expected_reference_outcome)

  #-------------------------------------------------------------------

  expected_mcmc_table <- data.frame(characteristic = c("Chains",
                                                       "Burn-in iterations",
                                                       "Sample iterations",
                                                       "Thinning factor"),
                                    value = c(4, 5000, 20000, 1))

  #Unit test 6
  expect_equal(GetBnmaMcmcCharacteristics(result_1), expected_mcmc_table)
  #-------------------------------------------------------------------

  expected_priors_table <- data.frame(parameter = c("Relative treatment effects",
                                                    "Intercepts",
                                                    "Heterogeneity standard deviation",
                                                    "Covariate mean",
                                                    "Covariate standard deviation"),
                                      value = c(" ~ N (0, 10000)",
                                                " ~ N (0, 10000)",
                                                " ~ Unif (0, 100)",
                                                " ~ N (0, 10000)",
                                                " ~ Unif (0, 100)")
  )
  #Unit test 7
  expect_equal(GetBnmaPriors(result_1), expected_priors_table)
})



test_that("BaselineRiskRelativeEffectsTable() has the correct format", {

  rel_eff_table <- matrix(c(NA, "[-0.1234,0,0.1753]", "[-2.3,-1,4]",
                            "[3,4,5]", NA, "[2,6,8.3]",
                            "[-7,4,11]", "[15.83125,16,17.0367]", NA), nrow = 3)
  rownames(rel_eff_table) <- c("TreatA", "TreatB", "TreatC")
  colnames(rel_eff_table) <- c("TreatA", "TreatB", "TreatC")

  expected_table <- matrix(c("TreatA", "0 (-0.12, 0.18)", "-1 (-2.3, 4)",
                             "4 (3, 5)", "TreatB", "6 (2, 8.3)",
                             "4 (-7, 11)", "16 (15.83, 17.04)", "TreatC"), nrow = 3)
  rownames(expected_table) <- c("TreatA", "TreatB", "TreatC")
  colnames(expected_table) <- c("TreatA", "TreatB", "TreatC")

  expect_equal(BaselineRiskRelativeEffectsTable(rel_eff_table), expected_table)
})



test_that("BnmaSwitchRanking() works", {

  ranking_table <- matrix(c(0.3, 0.1, 0.9,
                            0.5, 0.4, 0.06,
                            0.2, 0.5, 0.04), nrow = 3, byrow = TRUE)
  rownames(ranking_table) <- c("rank 1", "rank 2", "rank 3")
  colnames(ranking_table) <- c("TreatA", "TreatB", "TreatC")

  expected_table <- matrix(c(0.2, 0.5, 0.04,
                             0.5, 0.4, 0.06,
                             0.3, 0.1, 0.9), nrow = 3, byrow = TRUE)
  rownames(expected_table) <- c("rank 1", "rank 2", "rank 3")
  colnames(expected_table) <- c("TreatA", "TreatB", "TreatC")

  expect_equal(BnmaSwitchRanking(ranking_table), expected_table)
})



test_that("GetBnmaParameters returns the correct parameters", {
  all_parameters <- c("d[1]", "d[2]", "d[3]", "d[10]", "b_bl[1]", "b_bl[2]", "b_bl[3]", "sd", "sdB", "sd1", "delta[2]", "B_BL[3]")
  expected_parameters_fixed_shared <- c("d[2]", "d[3]", "d[10]", "b_bl[2]", "b_bl[3]")
  expected_parameters_random_unrelated <- c("d[2]", "d[3]", "d[10]", "b_bl[2]", "b_bl[3]", "sd")
  expected_parameters_fixed_exchangeable <- c("d[2]", "d[3]", "d[10]", "b_bl[2]", "b_bl[3]", "sdB")
  expected_parameters_random_exchangeable <- c("d[2]", "d[3]", "d[10]", "b_bl[2]", "b_bl[3]", "sd", "sdB")

  bnma_parameters_fixed_shared <- GetBnmaParameters(all_parameters = all_parameters,
                                                    effects_type = "fixed",
                                                    cov_parameters = "shared")
  bnma_parameters_random_unrelated <- GetBnmaParameters(all_parameters = all_parameters,
                                                        effects_type = "random",
                                                        cov_parameters = "unrelated")
  bnma_parameters_fixed_exchangeable <- GetBnmaParameters(all_parameters = all_parameters,
                                                          effects_type = "fixed",
                                                          cov_parameters = "exchangeable")
  bnma_parameters_random_exchangeable <- GetBnmaParameters(all_parameters = all_parameters,
                                                           effects_type = "random",
                                                           cov_parameters = "exchangeable")

  expect_equal(expected_parameters_fixed_shared, bnma_parameters_fixed_shared)
  expect_equal(expected_parameters_random_unrelated, bnma_parameters_random_unrelated)
  expect_equal(expected_parameters_fixed_exchangeable, bnma_parameters_fixed_exchangeable)
  expect_equal(expected_parameters_random_exchangeable, bnma_parameters_random_exchangeable)
})
