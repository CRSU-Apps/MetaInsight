
test_that("CreateVMatrix() works for binary outcomes and odds ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "OR"

  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 36.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 1/8 + 1/(40-8) + 1/13 + 1/(41-13)
  effect_variances[2] <- 1/14 + 1/(42-14) + 1/21 + 1/(43-21)
  effect_variances[3] <- 1/10 + 1/(44-10) + 1/15 + 1/(45-15)
  effect_variances[4] <- 1/10 + 1/(44-10) + 1/22 + 1/(46-22)
  effect_variances[5] <- 1/23 + 1/(47-23) + 1/30 + 1/(48-30)
  expected_v_matrix <- diag(effect_variances)
  expected_v_matrix[3, 4] <- 1/10 + 1/(44-10)
  expected_v_matrix[4, 3] <- expected_v_matrix[3, 4]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
    
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("CreateVMatrix() works for binary outcomes and risk ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RR"
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 34.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 1/8 - 1/40 + 1/13 - 1/41
  effect_variances[2] <- 1/14 - 1/42 + 1/21 - 1/43
  effect_variances[3] <- 1/10 - 1/44 + 1/15 - 1/45
  effect_variances[4] <- 1/10 - 1/44 + 1/22 - 1/46
  effect_variances[5] <- 1/23 - 1/47 + 1/30 - 1/48
  expected_v_matrix <- diag(effect_variances)
  expected_v_matrix[3, 4] <- 1/10 - 1/44
  expected_v_matrix[4, 3] <- expected_v_matrix[3, 4]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("CreateVMatrix() works for binary outcomes and risk differences", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RD"
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 37.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 8 * (40 - 8) / (40^3) + 13 * (41 - 13) / (41^3)
  effect_variances[2] <- 14 * (42 - 14) / (42^3) + 21 * (43 - 21) / (43^3)
  effect_variances[3] <- 10 * (44 - 10) / (44^3) + 15 * (45 - 15) / (45^3)
  effect_variances[4] <- 10 * (44 - 10) / (44^3) + 22 * (46 - 22) / (46^3)
  effect_variances[5] <- 23 * (47 - 23) / (47^3) + 30 * (48 - 30) / (48^3)
  expected_v_matrix <- diag(effect_variances)
  expected_v_matrix[3, 4] <- 10 * (44 - 10) / (44^3)
  expected_v_matrix[4, 3] <- expected_v_matrix[3, 4]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("CreateVMatrix() works for continuous outcomes", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     N = c(30, 31, 32, 33, 34, 35, 36, 37, 38),
                     Mean = c(20, 30, 31, 40, 21, 32, 41, 42, 50),
                     SD = c(5, 6, 6, 7, 7, 8, 9, 9, 10))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Continuous"
  outcome_measure <- "MD"
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 22, unpooled variance.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 5^2 / 30 + 6^2 / 31
  effect_variances[2] <- 6^2 / 32 + 7^2 / 33
  effect_variances[3] <- 7^2 / 34 + 8^2 / 35
  effect_variances[4] <- 7^2 / 34 + 9^2 / 36
  effect_variances[5] <- 9^2 / 37 + 10^2 / 38
  expected_v_matrix <- diag(effect_variances)
  expected_v_matrix[3, 4] <- 7^2 / 34
  expected_v_matrix[4, 3] <- expected_v_matrix[3, 4]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("CreateXMatrix() works for unrelated and exchangeable covariate parameters", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centred <- c(-5, -3, 2, 5)
  names(covar_centred) <- studies
  
  expected_x_matrix <- matrix(c(1, 0, 0, -5, 0, 0,   -1, 1, 0, -(-3), -3, 0,   1, 0, 0, 2, 0, 0,   0, 1, 0, 0, 2, 0,   0, -1, 1, 0, -5, -(-5)),
                     nrow = 5, ncol = 6, byrow = TRUE)
  rownames(expected_x_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_x_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Zinc_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Hydrogen:Zinc_beta")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "unrelated"), expected_x_matrix)
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "exchangeable"), expected_x_matrix)
})



test_that("CreateXMatrix() works for shared covariate parameters", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centred <- c(-5, -3, 2, 5)
  names(covar_centred) <- studies
  
  expected_x_matrix <- matrix(c(1, 0, 0, -5,   -1, 1, 0, 0,   1, 0, 0, 2,   0, 1, 0, 2,   0, -1, 1, 0),
                     nrow = 5, ncol = 4, byrow = TRUE)
  rownames(expected_x_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_x_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Zinc_d", "B")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "shared"), expected_x_matrix)
})



test_that("CreateZMatrix() works for unrelated and exchangeable covariate parameters", {

  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_z_matrix <- matrix(c(1, 0, 0, 0, 0, 0,   0, 1, 0, 0, 0, 0,   -1, 1, 0, 0, 0, 0,   0, 0, 1, 0, 0, 0,   -1, 0, 1, 0, 0, 0,   0, -1, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,   0, 0, 0, 0, 1, 0,   0, 0, 0, -1, 1, 0,   0, 0, 0, 0, 0, 1,   0, 0, 0, -1, 0, 1,   0, 0, 0, 0, -1, 1),
                     nrow = 12, ncol = 6, byrow = TRUE)
  rownames(expected_z_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Zinc_d", "Oxygen:Zinc_d", "Sulphur:Zinc_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta", "Hydrogen:Zinc_beta", "Oxygen:Zinc_beta", "Sulphur:Zinc_beta")
  colnames(expected_z_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Zinc_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Hydrogen:Zinc_beta")
  
  expect_equal(CreateZMatrix(treatments, "unrelated"), expected_z_matrix)
  expect_equal(CreateZMatrix(treatments, "exchangeable"), expected_z_matrix)
})



test_that("CreateZMatrix() works for shared covariate parameters", {
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_z_matrix <- matrix(c(1, 0, 0, 0,    0, 1, 0, 0,    -1, 1, 0, 0,    0, 0, 1, 0,    -1, 0, 1, 0,    0, -1, 1, 0,   0, 0, 0, 1),
                     nrow = 7, ncol = 4, byrow = TRUE)
  rownames(expected_z_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Zinc_d", "Oxygen:Zinc_d", "Sulphur:Zinc_d", "B")
  colnames(expected_z_matrix) <- c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Zinc_d", "B")
  
  expect_equal(CreateZMatrix(treatments, "shared"), expected_z_matrix)
})



test_that("CreateLambdaTauMatrix() works", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_lambda_tau_matrix <- diag(4, nrow = 5)
  expected_lambda_tau_matrix[3, 4] <- 2
  expected_lambda_tau_matrix[4, 3] <- 2
  rownames(expected_lambda_tau_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_lambda_tau_matrix) <- rownames(expected_lambda_tau_matrix)
  
  expect_equal(CreateLambdaTauMatrix(data, studies, treatments, 2), expected_lambda_tau_matrix)
})



test_that("CreateLambdaBetaMatrix() works", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_lambda_beta_matrix <- diag(4, nrow = 3)
  rownames(expected_lambda_beta_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc")
  colnames(expected_lambda_beta_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc")
  
  expect_equal(CreateLambdaBetaMatrix(treatments, 2), expected_lambda_beta_matrix)
})



test_that("CreateContributionMatrix() produces a matrix of the correct format for all three covariate parameter assumptions", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all")
  
  contribution_exchangeable <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "exchangeable", std_dev_beta = 1, study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all")
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_exchangeable), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_exchangeable), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when study_or_comparison_level = 'study'", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all")
  
  expect_equal(rownames(contribution_unrelated), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when basic_or_all_parameters = 'basic'", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "comparison", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "comparison", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage")
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "B"))
})