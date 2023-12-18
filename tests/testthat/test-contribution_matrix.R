
test_that("CreateVMatrix() works for binary outcomes", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(8, 13, 14, 21, 10, 15, 22, 23, 30),
                     N = c(40, 41, 42, 43, 44, 45, 46, 47, 48))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"

  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 36.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 1/8 + 1/(40-8) + 1/13 + 1/(41-13)
  effect_variances[2] <- 1/14 + 1/(42-14) + 1/21 + 1/(43-21)
  effect_variances[3] <- 1/10 + 1/(44-10) + 1/15 + 1/(45-15)
  effect_variances[4] <- 1/10 + 1/(44-10) + 1/22 + 1/(46-22)
  effect_variances[5] <- 1/23 + 1/(47-23) + 1/30 + 1/(48-30)
  V_matrix <- diag(effect_variances)
  V_matrix[3, 4] <- 1/10 + 1/(44-10)
  V_matrix[4, 3] <- V_matrix[3, 4]
  rownames(V_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(V_matrix) <- rownames(V_matrix)
    
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type), V_matrix)
})



test_that("CreateVMatrix() works for continuouos outcomes", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     N = c(30, 31, 32, 33, 34, 35, 36, 37, 38),
                     Mean = c(20, 30, 31, 40, 21, 32, 41, 42, 50),
                     SD = c(5, 6, 6, 7, 7, 8, 9, 9, 10))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Continuous"
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 22, unpooled variance.
  effect_variances <- vector(length=5)
  effect_variances[1] <- 5^2 / 30 + 6^2 / 31
  effect_variances[2] <- 6^2 / 32 + 7^2 / 33
  effect_variances[3] <- 7^2 / 34 + 8^2 / 35
  effect_variances[4] <- 7^2 / 34 + 9^2 / 36
  effect_variances[5] <- 9^2 / 37 + 10^2 / 38
  V_matrix <- diag(effect_variances)
  V_matrix[3, 4] <- 7^2 / 34
  V_matrix[4, 3] <- V_matrix[3, 4]
  rownames(V_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(V_matrix) <- rownames(V_matrix)
  
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type), V_matrix)
})



test_that("CreateXMatrix() works for unrelated and exchangeable covariate parameters", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(8, 13, 14, 21, 10, 15, 22, 23, 30),
                     N = c(40, 41, 42, 43, 44, 45, 46, 47, 48))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centered <- c(-5, -3, 2, 5)
  names(covar_centered) <- studies
  
  X_matrix <- matrix(c(1, 0, 0, -5, 0, 0,   -1, 1, 0, -(-3), -3, 0,   1, 0, 0, 2, 0, 0,   0, 1, 0, 0, 2, 0,   0, -1, 1, 0, -5, -(-5)),
                     nrow = 5, ncol = 6, byrow = TRUE)
  rownames(X_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(X_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Hydrogen:Zinc_beta")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centered, "Unrelated"), X_matrix)
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centered, "Exchangeable"), X_matrix)
})



test_that("CreateXMatrix() works for shared covariate parameters", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(8, 13, 14, 21, 10, 15, 22, 23, 30),
                     N = c(40, 41, 42, 43, 44, 45, 46, 47, 48))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centered <- c(-5, -3, 2, 5)
  names(covar_centered) <- studies
  
  X_matrix <- matrix(c(1, 0, 0, -5,   -1, 1, 0, 0,   1, 0, 0, 2,   0, 1, 0, 2,   0, -1, 1, 0),
                     nrow = 5, ncol = 4, byrow = TRUE)
  rownames(X_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(X_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc", "B")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centered, "Shared"), X_matrix)
})



test_that("CreateZMatrix() works for unrelated and exchangeable covariate parameters", {

  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  Z_matrix <- matrix(c(1, 0, 0, 0, 0, 0,   0, 1, 0, 0, 0, 0,   -1, 1, 0, 0, 0, 0,   0, 0, 1, 0, 0, 0,   -1, 0, 1, 0, 0, 0,   0, -1, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,   0, 0, 0, 0, 1, 0,   0, 0, 0, -1, 1, 0,   0, 0, 0, 0, 0, 1,   0, 0, 0, -1, 0, 1,   0, 0, 0, 0, -1, 1),
                     nrow = 12, ncol = 6, byrow = TRUE)
  rownames(Z_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "Hydrogen:Zinc", "Oxygen:Zinc", "Sulphur:Zinc",
                          "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta",
                          "Hydrogen:Zinc_beta", "Oxygen:Zinc_beta", "Sulphur:Zinc_beta")
  colnames(Z_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Hydrogen:Zinc_beta")
  
  expect_equal(CreateZMatrix(treatments, "Unrelated"), Z_matrix)
  expect_equal(CreateZMatrix(treatments, "Exchangeable"), Z_matrix)
})



test_that("CreateZMatrix() works for shared covariate parameters", {
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  Z_matrix <- matrix(c(1, 0, 0, 0,    0, 1, 0, 0,    -1, 1, 0, 0,    0, 0, 1, 0,    -1, 0, 1, 0,    0, -1, 1, 0,   0, 0, 0, 1),
                     nrow = 7, ncol = 4, byrow = TRUE)
  rownames(Z_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "Hydrogen:Zinc", "Oxygen:Zinc", "Sulphur:Zinc", "B")
  colnames(Z_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc", "B")
  
  expect_equal(CreateZMatrix(treatments, "Shared"), Z_matrix)
})



test_that("CreateLambdaTauMatrix() works", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(8, 13, 14, 21, 10, 15, 22, 23, 30),
                     N = c(40, 41, 42, 43, 44, 45, 46, 47, 48))
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  Lambda_tau_matrix <- diag(4, nrow = 5)
  Lambda_tau_matrix[3, 4] <- 2
  Lambda_tau_matrix[4, 3] <- 2
  rownames(Lambda_tau_matrix) <- c("(A)Hydrogen:Oxygen", "(B)Oxygen:Sulphur", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Sulphur:Zinc")
  colnames(Lambda_tau_matrix) <- rownames(Lambda_tau_matrix)
  
  expect_equal(CreateLambdaTauMatrix(data, studies, treatments, 2), Lambda_tau_matrix)
})



test_that("CreateLambdaBetaMatrix() works", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"),
                     R = c(8, 13, 14, 21, 10, 15, 22, 23, 30),
                     N = c(40, 41, 42, 43, 44, 45, 46, 47, 48))
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  Lambda_beta_matrix <- diag(4, nrow = 3)
  rownames(Lambda_beta_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc")
  colnames(Lambda_beta_matrix) <- c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Zinc")
  
  expect_equal(CreateLambdaBetaMatrix(treatments, 2), Lambda_beta_matrix)
})



test_that("CreateContributionMatrix() produces a matrix of the correct format for all three covariate parameter assumptions", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 1, 2, 1, 2, 3, 1, 3),
                     R = c(8, 13, 9, 14, 10, 15, 20, 11, 21),
                     N = rep(40, times = 9),
                     covar.test = c(-0.5, -0.5, -0.3, -0.3, 0.2, 0.2, 0.2, 0.5, 0.5))
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Unrelated", study_level = FALSE, percentages = TRUE)
  
  contribution_exchangeable <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Exchangeable", std_dev_beta = 1, study_level = FALSE, percentages = TRUE)
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Shared", study_level = FALSE, percentages = TRUE)
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_exchangeable), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_exchangeable), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when study_level = TRUE", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 1, 2, 1, 2, 3, 1, 3),
                     R = c(8, 13, 9, 14, 10, 15, 20, 11, 21),
                     N = rep(40, times = 9),
                     covar.test = c(-0.5, -0.5, -0.3, -0.3, 0.2, 0.2, 0.2, 0.5, 0.5))
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Unrelated", study_level = TRUE, percentages = TRUE)
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Shared", study_level = TRUE, percentages = TRUE)
  
  expect_equal(rownames(contribution_unrelated), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta", "Oxygen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Oxygen:Sulphur", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when all_parameters = FALSE", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 1, 2, 1, 2, 3, 1, 3),
                     R = c(8, 13, 9, 14, 10, 15, 20, 11, 21),
                     N = rep(40, times = 9),
                     covar.test = c(-0.5, -0.5, -0.3, -0.3, 0.2, 0.2, 0.2, 0.5, 0.5))
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Unrelated", study_level = FALSE, all_parameters = FALSE, percentages = TRUE)
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", effects_type = "Fixed", cov_parameters = "Shared", study_level = FALSE, all_parameters = FALSE, percentages = TRUE)
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "Hydrogen:Oxygen_beta", "Hydrogen:Sulphur_beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen:Oxygen", "(B)Hydrogen:Oxygen", "(C)Hydrogen:Oxygen", "(C)Hydrogen:Sulphur", "(D)Hydrogen:Sulphur"))
  expect_equal(colnames(contribution_shared), c("Hydrogen:Oxygen", "Hydrogen:Sulphur", "B"))
})