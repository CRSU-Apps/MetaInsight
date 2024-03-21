
test_that("GetEffectSizesAndVariances() and CreateVMatrix() work for binary outcomes and odds ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "OR"

  expected_effects <- vector(length = 5)
  expected_effects[1] <- log(8 / (40 - 8)) - log(13 / (41 - 13))
  expected_effects[2] <- log(14 / (42 - 14)) - log(21 / (43 - 21))
  expected_effects[3] <- log(10 / (44 - 10)) - log(15 / (45 - 15))
  expected_effects[4] <- log(10 / (44 - 10)) - log(22 / (46 - 22))
  expected_effects[5] <- log(23 / (47 - 23)) - log(30 / (48 - 30))
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 36.
  expected_effect_variances <- vector(length=5)
  expected_effect_variances[1] <- 1/8 + 1/(40-8) + 1/13 + 1/(41-13)
  expected_effect_variances[2] <- 1/14 + 1/(42-14) + 1/21 + 1/(43-21)
  expected_effect_variances[3] <- 1/10 + 1/(44-10) + 1/15 + 1/(45-15)
  expected_effect_variances[4] <- 1/10 + 1/(44-10) + 1/22 + 1/(46-22)
  expected_effect_variances[5] <- 1/23 + 1/(47-23) + 1/30 + 1/(48-30)
  
  expected_effect_sizes <- data.frame(Study = c("A", "B", "C", "C", "D"),
                                      Treatment = c("Oxygen", "Sulphur", "Oxygen", "Sulphur", "Zinc"),
                                      Effect = expected_effects,
                                      Variance = expected_effect_variances)
  
  expected_control_var <- vector(length = 4)
  expected_control_var[1] <- 1/8 + 1/(40 - 8)
  expected_control_var[2] <- 1/14 + 1/(42 - 14)
  expected_control_var[3] <- 1/10 + 1/(44 - 10)
  expected_control_var[4] <- 1/23 + 1/(47 - 23)
  
  expected_control_variance <- data.frame(Study = c("A", "B", "C", "D"),
                                          Treatment = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"),
                                          Variance = expected_control_var)
  
  expected_v_matrix <- diag(expected_effect_variances)
  expected_v_matrix[3, 4] <- expected_control_var[3]
  expected_v_matrix[4, 3] <- expected_control_var[3]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
    
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$effect_sizes, expected_effect_sizes)
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$control_variance, expected_control_variance)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetEffectSizesAndVariances() and CreateVMatrix() work for binary outcomes and risk ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RR"
  
  expected_effects <- vector(length = 5)
  expected_effects[1] <- log(8 / 40) - log(13 / 41)
  expected_effects[2] <- log(14 / 42) - log(21 / 43)
  expected_effects[3] <- log(10 / 44) - log(15 / 45)
  expected_effects[4] <- log(10 / 44) - log(22 / 46)
  expected_effects[5] <- log(23 / 47) - log(30 / 48)

  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 34.
  expected_effect_variances <- vector(length=5)
  expected_effect_variances[1] <- 1/8 - 1/40 + 1/13 - 1/41
  expected_effect_variances[2] <- 1/14 - 1/42 + 1/21 - 1/43
  expected_effect_variances[3] <- 1/10 - 1/44 + 1/15 - 1/45
  expected_effect_variances[4] <- 1/10 - 1/44 + 1/22 - 1/46
  expected_effect_variances[5] <- 1/23 - 1/47 + 1/30 - 1/48
  
  expected_effect_sizes <- data.frame(Study = c("A", "B", "C", "C", "D"),
                                      Treatment = c("Oxygen", "Sulphur", "Oxygen", "Sulphur", "Zinc"),
                                      Effect = expected_effects,
                                      Variance = expected_effect_variances)
  
  expected_control_var <- vector(length = 4)
  expected_control_var[1] <- 1/8 - 1/40
  expected_control_var[2] <- 1/14 - 1/42
  expected_control_var[3] <- 1/10 - 1/44
  expected_control_var[4] <- 1/23 - 1/47
  
  expected_control_variance <- data.frame(Study = c("A", "B", "C", "D"),
                                          Treatment = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"),
                                          Variance = expected_control_var)
  
  expected_v_matrix <- diag(expected_effect_variances)
  expected_v_matrix[3, 4] <- expected_control_var[3]
  expected_v_matrix[4, 3] <- expected_control_var[3]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$effect_sizes, expected_effect_sizes)
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$control_variance, expected_control_variance)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetEffectSizesAndVariances() and CreateVMatrix() work for binary outcomes and risk differences", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RD"
  
  expected_effects <- vector(length = 5)
  expected_effects[1] <- (8 / 40) - (13 / 41)
  expected_effects[2] <- (14 / 42) - (21 / 43)
  expected_effects[3] <- (10 / 44) - (15 / 45)
  expected_effects[4] <- (10 / 44) - (22 / 46)
  expected_effects[5] <- (23 / 47) - (30 / 48)
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 37.
  expected_effect_variances <- vector(length=5)
  expected_effect_variances[1] <- 8 * (40 - 8) / (40^3) + 13 * (41 - 13) / (41^3)
  expected_effect_variances[2] <- 14 * (42 - 14) / (42^3) + 21 * (43 - 21) / (43^3)
  expected_effect_variances[3] <- 10 * (44 - 10) / (44^3) + 15 * (45 - 15) / (45^3)
  expected_effect_variances[4] <- 10 * (44 - 10) / (44^3) + 22 * (46 - 22) / (46^3)
  expected_effect_variances[5] <- 23 * (47 - 23) / (47^3) + 30 * (48 - 30) / (48^3)
  
  expected_effect_sizes <- data.frame(Study = c("A", "B", "C", "C", "D"),
                                      Treatment = c("Oxygen", "Sulphur", "Oxygen", "Sulphur", "Zinc"),
                                      Effect = expected_effects,
                                      Variance = expected_effect_variances)
  
  expected_control_var <- vector(length = 4)
  expected_control_var[1] <- 8 * (40 - 8) / 40^3
  expected_control_var[2] <- 14 * (42 - 14) / 42^3
  expected_control_var[3] <- 10 * (44 - 10) / 44^3
  expected_control_var[4] <- 23 * (47 - 23) / 47^3
  
  expected_control_variance <- data.frame(Study = c("A", "B", "C", "D"),
                                          Treatment = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"),
                                          Variance = expected_control_var)
  
  expected_v_matrix <- diag(expected_effect_variances)
  expected_v_matrix[3, 4] <- expected_control_var[3]
  expected_v_matrix[4, 3] <- expected_control_var[3]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$effect_sizes, expected_effect_sizes)
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$control_variance, expected_control_variance)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetEffectSizesAndVariances() and CreateVMatrix() work for continuous outcomes", {
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
  
  expected_effects <- vector(length = 5)
  expected_effects[1] <- 20 - 30
  expected_effects[2] <- 31 - 40
  expected_effects[3] <- 21 - 32
  expected_effects[4] <- 21 - 41
  expected_effects[5] <- 42 - 50
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 22, unpooled variance.
  expected_effect_variances <- vector(length=5)
  expected_effect_variances[1] <- 5^2 / 30 + 6^2 / 31
  expected_effect_variances[2] <- 6^2 / 32 + 7^2 / 33
  expected_effect_variances[3] <- 7^2 / 34 + 8^2 / 35
  expected_effect_variances[4] <- 7^2 / 34 + 9^2 / 36
  expected_effect_variances[5] <- 9^2 / 37 + 10^2 / 38
  
  expected_effect_sizes <- data.frame(Study = c("A", "B", "C", "C", "D"),
                                      Treatment = c("Oxygen", "Sulphur", "Oxygen", "Sulphur", "Zinc"),
                                      Effect = expected_effects,
                                      Variance = expected_effect_variances)
  
  expected_control_var <- vector(length = 4)
  expected_control_var[1] <- 5^2 / 30
  expected_control_var[2] <- 6^2 / 32
  expected_control_var[3] <- 7^2 / 34
  expected_control_var[4] <- 9^2 / 37
  
  expected_control_variance <- data.frame(Study = c("A", "B", "C", "D"),
                                          Treatment = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"),
                                          Variance = expected_control_var)
  
  expected_v_matrix <- diag(expected_effect_variances)
  expected_v_matrix[3, 4] <- expected_control_var[3]
  expected_v_matrix[4, 3] <- expected_control_var[3]
  rownames(expected_v_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$effect_sizes, expected_effect_sizes)
  expect_equal(GetEffectSizesAndVariances(data, treatments, outcome_type, outcome_measure)$control_variance, expected_control_variance)
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



test_that("CheckSingularMatrix() works", {
  invertible_matrix <- matrix(c(1:3, 8:6, c(0, 0, 1)), nrow = 3)
  singular_matrix <- matrix(c(1:3, 1:3, 4:6), nrow = 3)
  
  expect_equal(CheckSingularMatrix(invertible_matrix), NULL)
  expect_error(CheckSingularMatrix(singular_matrix))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format for all three covariate parameter assumptions", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_exchangeable <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "exchangeable", std_dev_beta = 1, study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "comparison", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
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
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  expect_equal(rownames(contribution_unrelated), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Oxygen:Sulphur_d", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when basic_or_all_parameters = 'basic'", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_comparison_level = "comparison", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_comparison_level = "comparison", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage", weight_or_contribution = "weight")
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen_d", "Hydrogen:Sulphur_d", "B"))
})



test_that("CreateContributionMatrix() produces correct output for the Donegan example", {
  #Load data
  donegan_original <- read.csv("Donegan_contribution_matrix_data.csv")
  #Make long format
  donegan_t1 <- donegan_original[, c("s", "t1", "x")]
  donegan_t2 <- donegan_original[, c("s", "t2", "x")]
  names(donegan_t1)[names(donegan_t1) == "t1"] <- "T"
  names(donegan_t2)[names(donegan_t2) == "t2"] <- "T"
  donegan <- rbind(donegan_t1, donegan_t2)
  #Create Study column
  donegan$Study <- paste0("Study ", formatC(donegan$s, digits = 1, flag = 0))
  #Create Treatment column
  donegan$Treatment <- donegan$T
  #Rename covariate
  names(donegan)[names(donegan) == "x"] <- "covar.x"
  donegan <- donegan[order(donegan$Study, donegan$Treatment), ]
  donegan_studies <- unique(donegan$Study)
  donegan_covariate <- donegan_t1$x
  names(donegan_covariate) <- donegan_studies
  #Create treatment IDs
  donegan_ids <- data.frame(Number = 1:3, Label = as.character(1:3))
  donegan <- donegan[, -which(names(donegan) == "s")]
  
  #Create backups of these two functions
  BackupCreateVMatrix <- CreateVMatrix
  BackupGetEffectSizesAndVariances <- GetEffectSizesAndVariances
  
  #Overwrite CreateVMatrix(), because the example is contrast-level rather than arm-level
  CreateVMatrix <<- function(data, studies, treatments, outcome_type, outcome_measure){
    V <- diag(donegan_original$se^2)
    rownames(V) <- donegan_studies
    colnames(V) <- donegan_studies
    return(V)
  }
  
  #Overwrite GetEffectSizesAndVariances(), because the example is contrast-level rather than arm-level
  GetEffectSizesAndVariances <<- function(data, treatments, outcome_type, outcome_measure){
    return(list(effect_sizes = data.frame(Effect = donegan_original$lor)))
  }
  
  #Restore the functions to their original definitions at the end
  on.exit(CreateVMatrix <<- BackupCreateVMatrix, add = TRUE, after = FALSE)
  on.exit(GetEffectSizesAndVariances <<- BackupGetEffectSizesAndVariances, add = TRUE, after = FALSE)
  
  contribution_matrix <- CreateContributionMatrix(data = donegan,
                                                  treatment_ids = donegan_ids,
                                                  outcome_type = "Binary",                           
                                                  outcome_measure = "OR",
                                                  effects_type = "fixed",
                                                  cov_parameters = "unrelated",
                                                  basic_or_all_parameters = "all",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "percentage",
                                                  weight_or_contribution = "weight",
                                                  full_output = FALSE)
  
  expected_contribution_matrix <- read.csv("Donegan_contribution_matrix_expected.csv")
  expected_contribution_matrix <- as.matrix(expected_contribution_matrix[, 2:7])
  colnames(expected_contribution_matrix) <- c("1:2_d", "1:3_d", "2:3_d", "1:2_beta", "1:3_beta", "2:3_beta")
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() produces correct output for a fixed effects, shared model", {
  data <- read.csv("Cont_long_cont_cov_small.csv")

  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  expected_V <- matrix(c(2^2/50 + 2^2/51, 0, 0,
                         0, 3^2/60 + 3^2/61, 3^2/60,
                         0, 3^2/60, 3^2/62 + 3^2/60),
                       byrow = TRUE, nrow = 3)
  expected_X <- matrix(c(1, 0, 1-1.25,
                         1, 0, 1.5-1.25,
                         0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 3)
  expected_XVX_matrix <- solve(t(expected_X)%*% solve(expected_V) %*% expected_X) %*% t(expected_X) %*% solve(expected_V)
  expected_contribution_matrix <- round(t(abs(expected_XVX_matrix)), digits = 2)
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  colnames(expected_contribution_matrix) <- colnames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() produces correct output for a fixed effects, exchangeable model", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "exchangeable",
                                                  std_dev_beta = 0.9,
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  expected_V <- matrix(c(2^2/50 + 2^2/51, 0, 0,
                         0, 3^2/60 + 3^2/61, 3^2/60,
                         0, 3^2/60, 3^2/62 + 3^2/60),
                       byrow = TRUE, nrow = 3)
  expected_lambda_beta <- diag(0.9^2, nrow = 2)
  expected_V_star <- rbind(cbind(expected_V, matrix(rep(0, times = 6), nrow = 3)),
                           cbind(matrix(rep(0, times = 6), nrow = 2), expected_lambda_beta))
  
  expected_X_d <- matrix(c(1, 0,
                           1, 0,
                           0, 1),
                         byrow = TRUE, nrow = 3)
  expected_X_beta <- matrix(c(1-1.25, 0,
                              1.5-1.25, 0,
                              0, 1.5-1.25),
                            byrow = TRUE, nrow = 3)
  expected_X_star <- rbind(cbind(expected_X_d, expected_X_beta, matrix(rep(0, times = 3), nrow = 3)),
                           cbind(matrix(rep(0, times = 4), nrow = 2), diag(1, nrow = 2), matrix(rep(1, times = 2), nrow = 2)))
  expected_XVX_matrix <- solve(t(expected_X_star)%*% solve(expected_V_star) %*% expected_X_star) %*% t(expected_X_star) %*% solve(expected_V_star)
  expected_A_matrix <- expected_XVX_matrix[1:4, 1:3]
  expected_contribution_matrix <- round(t(abs(expected_A_matrix)), digits = 2)
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  colnames(expected_contribution_matrix) <- colnames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() produces correct output for a random effects, shared model", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "random",
                                                  cov_parameters = "shared",
                                                  std_dev_d = 0.9,
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  expected_V <- matrix(c(2^2/50 + 2^2/51, 0, 0,
                         0, 3^2/60 + 3^2/61, 3^2/60,
                         0, 3^2/60, 3^2/62 + 3^2/60),
                       byrow = TRUE, nrow = 3)
  expected_lambda_tau <- matrix(c(0.9^2, 0, 0,
                                  0, 0.9^2, 0.9^2/2,
                                  0, 0.9^2/2, 0.9^2),
                                byrow = TRUE, nrow = 3)
  expected_V_star <- rbind(cbind(expected_V, matrix(rep(0, times = 9), nrow = 3)),
                           cbind(matrix(rep(0, times = 9), nrow = 3), expected_lambda_tau))
  
  expected_X <- matrix(c(1, 0, 1-1.25,
                         1, 0, 1.5-1.25,
                         0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 3)
  expected_X_star <- rbind(cbind(diag(1, nrow = 3), matrix(rep(0, times = 9), nrow = 3)),
                           cbind(diag(1, nrow = 3), -expected_X))
  expected_XVX_matrix <- solve(t(expected_X_star)%*% solve(expected_V_star) %*% expected_X_star) %*% t(expected_X_star) %*% solve(expected_V_star)
  expected_A_matrix <- expected_XVX_matrix[4:6, 1:3]
  expected_contribution_matrix <- round(t(abs(expected_A_matrix)), digits = 2)
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  colnames(expected_contribution_matrix) <- colnames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() includes all parameters when 'basic_or_all_parameters' = 'all'", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "all",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  basic_contribution_matrix <- CreateContributionMatrix(data = data,
                                                        treatment_ids = treatment_ids,
                                                        outcome_type = "Continuous",                           
                                                        outcome_measure = "MD",
                                                        effects_type = "fixed",
                                                        cov_parameters = "shared",
                                                        basic_or_all_parameters = "basic",
                                                        study_or_comparison_level = "comparison",
                                                        absolute_or_percentage = "absolute",
                                                        weight_or_contribution = "weight")
  
  expected_V <- matrix(c(2^2/50 + 2^2/51, 0, 0,
                         0, 3^2/60 + 3^2/61, 3^2/60,
                         0, 3^2/60, 3^2/62 + 3^2/60),
                       byrow = TRUE, nrow = 3)
  expected_X <- matrix(c(1, 0, 1-1.25,
                         1, 0, 1.5-1.25,
                         0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 3)
  expected_Z <- matrix(c(1, 0, 0,
                         0, 1, 0,
                         -1, 1, 0,
                         0, 0, 1),
                       byrow = TRUE, nrow = 4)
  expected_XVX_matrix <- solve(t(expected_X)%*% solve(expected_V) %*% expected_X) %*% t(expected_X) %*% solve(expected_V)
  expected_contribution_matrix <- round(t(abs(expected_Z %*% expected_XVX_matrix)), digits = 2)
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  colnames(expected_contribution_matrix) <- colnames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() adds up rows correctly when 'study_or_comparison_level' = 'study'", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "study",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  comparison_matrix <- CreateContributionMatrix(data = data,
                                                treatment_ids = treatment_ids,
                                                outcome_type = "Continuous",                           
                                                outcome_measure = "MD",
                                                effects_type = "fixed",
                                                cov_parameters = "shared",
                                                basic_or_all_parameters = "basic",
                                                study_or_comparison_level = "comparison",
                                                absolute_or_percentage = "absolute",
                                                weight_or_contribution = "weight")
  
  expected_contribution_matrix <- rbind(comparison_matrix[1, ],
                                        colSums(comparison_matrix[2:3, ]))
  rownames(expected_contribution_matrix) <- c("A", "B")
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() calculates percentages correctly", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "percentage",
                                                  weight_or_contribution = "weight")
  
  absolute_contribution_matrix <- CreateContributionMatrix(data = data,
                                                           treatment_ids = treatment_ids,
                                                           outcome_type = "Continuous",                           
                                                           outcome_measure = "MD",
                                                           effects_type = "fixed",
                                                           cov_parameters = "shared",
                                                           basic_or_all_parameters = "basic",
                                                           study_or_comparison_level = "comparison",
                                                           absolute_or_percentage = "absolute",
                                                           weight_or_contribution = "weight")
  
  column_totals <- colSums(absolute_contribution_matrix)
  column_totals_matrix <- matrix(rep(column_totals, times = 3), byrow = TRUE, nrow = 3)
  expected_contribution_matrix <- 100 * absolute_contribution_matrix / column_totals_matrix
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() calculates contributions correctly when 'weight_or_contribution' = 'contribution'", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "basic",
                                                  study_or_comparison_level = "comparison",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "contribution")
  
  weight_matrix <- CreateContributionMatrix(data = data,
                                            treatment_ids = treatment_ids,
                                            outcome_type = "Continuous",                           
                                            outcome_measure = "MD",
                                            effects_type = "fixed",
                                            cov_parameters = "shared",
                                            basic_or_all_parameters = "basic",
                                            study_or_comparison_level = "comparison",
                                            absolute_or_percentage = "absolute",
                                            weight_or_contribution = "weight")
  
  treatment_effects <- c(6 - 5, 7 - 6, 8 - 6)
  treatment_effects_matrix <- matrix(rep(treatment_effects, times = 3), nrow = 3)
  expected_contribution_matrix <- weight_matrix * treatment_effects_matrix
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})
