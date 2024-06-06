
test_that("GetOutcomesAndVariances() and CreateVMatrix() work for binary outcomes and odds ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "OR"

  expected_outcomes <- vector(length = 9)
  expected_outcomes[1] <- log(8 / (40 - 8))
  expected_outcomes[2] <- log(13 / (41 - 13))
  expected_outcomes[3] <- log(14 / (42 - 14))
  expected_outcomes[4] <- log(21 / (43 - 21))
  expected_outcomes[5] <- log(10 / (44 - 10))
  expected_outcomes[6] <- log(15 / (45 - 15))
  expected_outcomes[7] <- log(22 / (46 - 22))
  expected_outcomes[8] <- log(23 / (47 - 23))
  expected_outcomes[9] <- log(30 / (48 - 30))
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 36.
  expected_variances <- vector(length = 9)
  expected_variances[1] <- 1/8 + 1/(40-8)
  expected_variances[2] <- 1/13 + 1/(41-13)
  expected_variances[3] <- 1/14 + 1/(42-14)
  expected_variances[4] <- 1/21 + 1/(43-21)
  expected_variances[5] <- 1/10 + 1/(44-10)
  expected_variances[6] <- 1/15 + 1/(45-15)
  expected_variances[7] <- 1/22 + 1/(46-22)
  expected_variances[8] <- 1/23 + 1/(47-23)
  expected_variances[9] <- 1/30 + 1/(48-30)
  
  expected_outcomes_and_variances <- data.frame(Study = data$Study,
                                                Treatment = data$Treatment,
                                                Outcome = expected_outcomes,
                                                Variance = expected_variances)
  
  expected_v_matrix <- diag(expected_variances)

  rownames(expected_v_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur",
                                   "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
    
  expect_equal(GetOutcomesAndVariances(data, treatments, outcome_type, outcome_measure), expected_outcomes_and_variances)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetOutcomesAndVariances() and CreateVMatrix() work for binary outcomes and risk ratios", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RR"
  
  expected_outcomes <- vector(length = 9)
  expected_outcomes[1] <- log(8 / 40)
  expected_outcomes[2] <- log(13 / 41)
  expected_outcomes[3] <- log(14 / 42)
  expected_outcomes[4] <- log(21 / 43)
  expected_outcomes[5] <- log(10 / 44)
  expected_outcomes[6] <- log(15 / 45)
  expected_outcomes[7] <- log(22 / 46)
  expected_outcomes[8] <- log(23 / 47)
  expected_outcomes[9] <- log(30 / 48)

  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 34.
  expected_variances <- vector(length = 9)
  expected_variances[1] <- 1/8 - 1/40
  expected_variances[2] <- 1/13 - 1/41
  expected_variances[3] <- 1/14 - 1/42
  expected_variances[4] <- 1/21 - 1/43
  expected_variances[5] <- 1/10 - 1/44
  expected_variances[6] <- 1/15 - 1/45
  expected_variances[7] <- 1/22 - 1/46
  expected_variances[8] <- 1/23 - 1/47
  expected_variances[9] <- 1/30 - 1/48
  
  expected_outcomes_and_variances <- data.frame(Study = data$Study,
                                                Treatment = data$Treatment,
                                                Outcome = expected_outcomes,
                                                Variance = expected_variances)
  
  expected_v_matrix <- diag(expected_variances)

  rownames(expected_v_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetOutcomesAndVariances(data, treatments, outcome_type, outcome_measure), expected_outcomes_and_variances)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetOutcomesAndVariances() and CreateVMatrix() work for binary outcomes and risk differences", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  outcome_type <- "Binary"
  outcome_measure <- "RD"
  
  expected_outcomes <- vector(length = 9)
  expected_outcomes[1] <- 8 / 40 
  expected_outcomes[2] <- 13 / 41
  expected_outcomes[3] <- 14 / 42
  expected_outcomes[4] <- 21 / 43
  expected_outcomes[5] <- 10 / 44
  expected_outcomes[6] <- 15 / 45
  expected_outcomes[7] <- 22 / 46
  expected_outcomes[8] <- 23 / 47
  expected_outcomes[9] <- 30 / 48
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 37.
  expected_variances <- vector(length = 9)
  expected_variances[1] <- 8 * (40 - 8) / (40^3)
  expected_variances[2] <- 13 * (41 - 13) / (41^3)
  expected_variances[3] <- 14 * (42 - 14) / (42^3)
  expected_variances[4] <- 21 * (43 - 21) / (43^3)
  expected_variances[5] <- 10 * (44 - 10) / (44^3)
  expected_variances[6] <- 15 * (45 - 15) / (45^3)
  expected_variances[7] <- 22 * (46 - 22) / (46^3)
  expected_variances[8] <- 23 * (47 - 23) / (47^3)
  expected_variances[9] <- 30 * (48 - 30) / (48^3)
  
  expected_outcomes_and_variances <- data.frame(Study = data$Study,
                                                Treatment = data$Treatment,
                                                Outcome = expected_outcomes,
                                                Variance = expected_variances)
  
  expected_v_matrix <- diag(expected_variances)

  rownames(expected_v_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetOutcomesAndVariances(data, treatments, outcome_type, outcome_measure), expected_outcomes_and_variances)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("GetOutcomesAndVariances() and CreateVMatrix() work for continuous outcomes", {
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
  
  expected_outcomes <- data$Mean
  
  #Variance formula from "Introducion to Meta-Analysis" 2nd edition, Borenstein et al, page 22, unpooled variance.
  expected_variances <- vector(length = 9)
  expected_variances[1] <- 5^2 / 30
  expected_variances[2] <- 6^2 / 31
  expected_variances[3] <- 6^2 / 32
  expected_variances[4] <- 7^2 / 33
  expected_variances[5] <- 7^2 / 34
  expected_variances[6] <- 8^2 / 35
  expected_variances[7] <- 9^2 / 36
  expected_variances[8] <- 9^2 / 37
  expected_variances[9] <- 10^2 / 38
  
  expected_outcomes_and_variances <- data.frame(Study = data$Study,
                                                Treatment = data$Treatment,
                                                Outcome = expected_outcomes,
                                                Variance = expected_variances)
  
  expected_v_matrix <- diag(expected_variances)

  rownames(expected_v_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_v_matrix) <- rownames(expected_v_matrix)
  
  expect_equal(GetOutcomesAndVariances(data, treatments, outcome_type, outcome_measure), expected_outcomes_and_variances)
  expect_equal(CreateVMatrix(data, studies, treatments, outcome_type, outcome_measure), expected_v_matrix)
})



test_that("CreateXMatrix() works for unrelated and exchangeable covariate parameters", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centred <- c(-5, -3, 2, 5)
  names(covar_centred) <- studies
  
  expected_x_matrix <- matrix(c(1, 0, 0, 0,  0,  0, 0,  0,  0, 0,
                                1, 0, 0, 0,  1,  0, 0, -5,  0, 0,
                                0, 1, 0, 0,  0,  0, 0,  0,  0, 0,
                                0, 1, 0, 0, -1,  1, 0,  3, -3, 0,
                                0, 0, 1, 0,  0,  0, 0,  0,  0, 0,
                                0, 0, 1, 0,  1,  0, 0,  2,  0, 0,
                                0, 0, 1, 0,  0,  1, 0,  0,  2, 0,
                                0, 0, 0, 1,  0,  0, 0,  0,  0, 0,
                                0, 0, 0, 1,  0, -1, 1,  0, -5, 5),
                     nrow = 9, ncol = 10, byrow = TRUE)
  rownames(expected_x_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_x_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Hydrogen:Zinc-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Hydrogen:Zinc-beta")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "unrelated"), expected_x_matrix)
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "exchangeable"), expected_x_matrix)
})



test_that("CreateXMatrix() works for shared covariate parameters", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  covar_centred <- c(-5, -3, 2, 5)
  names(covar_centred) <- studies
  
  expected_x_matrix <- matrix(c(1, 0, 0, 0,  0,  0, 0,  0,
                                1, 0, 0, 0,  1,  0, 0, -5,
                                0, 1, 0, 0,  0,  0, 0,  0,
                                0, 1, 0, 0, -1,  1, 0,  0,
                                0, 0, 1, 0,  0,  0, 0,  0,
                                0, 0, 1, 0,  1,  0, 0,  2,
                                0, 0, 1, 0,  0,  1, 0,  2,
                                0, 0, 0, 1,  0,  0, 0,  0,
                                0, 0, 0, 1,  0, -1, 1,  0),
                              nrow = 9, ncol = 8, byrow = TRUE)
  rownames(expected_x_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
  colnames(expected_x_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Hydrogen:Zinc-d", "B")
  
  expect_equal(CreateXMatrix(data, studies, treatments, covar_centred, "shared"), expected_x_matrix)
})



test_that("CreateZMatrix() works for unrelated and exchangeable covariate parameters", {

  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_Z_top_left <- diag(1, nrow = 4)
  expected_Z_middle <- matrix(c( 1,  0, 0,
                                 0,  1, 0,
                                -1,  1, 0,
                                 0,  0, 1,
                                -1,  0, 1,
                                 0, -1, 1),
                              nrow = 6, ncol = 3, byrow = TRUE)
  expected_Z_bottom_right <- expected_Z_middle
  expected_Z_matrix <- as.matrix(bdiag(expected_Z_top_left,
                                       expected_Z_middle,
                                       expected_Z_bottom_right
                                       )
                                 )

  rownames(expected_Z_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "Hydrogen:Zinc-d", "Oxygen:Zinc-d", "Sulphur:Zinc-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Oxygen:Sulphur-beta", "Hydrogen:Zinc-beta", "Oxygen:Zinc-beta", "Sulphur:Zinc-beta")
  colnames(expected_Z_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Hydrogen:Zinc-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Hydrogen:Zinc-beta")
  
  expect_equal(CreateZMatrix(studies, treatments, "unrelated"), expected_Z_matrix)
  expect_equal(CreateZMatrix(studies, treatments, "exchangeable"), expected_Z_matrix)
})



test_that("CreateZMatrix() works for shared covariate parameters", {
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_Z_top_left <- diag(1, nrow = 4)
  expected_Z_middle <- matrix(c( 1,  0, 0,
                                 0,  1, 0,
                                -1,  1, 0,
                                 0,  0, 1,
                                -1,  0, 1,
                                 0, -1, 1),
                              nrow = 6, ncol = 3, byrow = TRUE)
  expected_Z_bottom_right <- matrix(1)
  expected_Z_matrix <- as.matrix(bdiag(expected_Z_top_left,
                                       expected_Z_middle,
                                       expected_Z_bottom_right
                                       )
                                 )
  rownames(expected_Z_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "Hydrogen:Zinc-d", "Oxygen:Zinc-d", "Sulphur:Zinc-d", "B")
  colnames(expected_Z_matrix) <- c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Hydrogen:Zinc-d", "B")
  
  expect_equal(CreateZMatrix(studies, treatments, "shared"), expected_Z_matrix)
})



test_that("CreateLambdaTauMatrix() works", {
  data <- read.csv("Binary_long_for_contribution_matrix.csv")
  
  studies <- c("A", "B", "C", "D")
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  expected_lambda_tau_matrix <- diag(4, nrow = 9)

  rownames(expected_lambda_tau_matrix) <- c("(A)Hydrogen", "(A)Oxygen", "(B)Oxygen", "(B)Sulphur", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Sulphur", "(D)Zinc")
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
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_arm_level = "arm", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_exchangeable <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "exchangeable", std_dev_beta = 1, study_or_arm_level = "arm", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_arm_level = "arm", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen", "(A)Oxygen", "(B)Hydrogen", "(B)Oxygen", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Hydrogen", "(D)Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Oxygen:Sulphur-beta"))
  
  expect_equal(rownames(contribution_exchangeable), c("(A)Hydrogen", "(A)Oxygen", "(B)Hydrogen", "(B)Oxygen", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Hydrogen", "(D)Sulphur"))
  expect_equal(colnames(contribution_exchangeable), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Oxygen:Sulphur-beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen", "(A)Oxygen", "(B)Hydrogen", "(B)Oxygen", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Hydrogen", "(D)Sulphur"))
  expect_equal(colnames(contribution_shared), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when study_or_arm_level = 'study'", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_arm_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_arm_level = "study", absolute_or_percentage = "percentage", basic_or_all_parameters = "all", weight_or_contribution = "weight")
  
  expect_equal(rownames(contribution_unrelated), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_unrelated), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta", "Oxygen:Sulphur-beta"))
  
  expect_equal(rownames(contribution_shared), c("A", "B", "C", "D"))
  expect_equal(colnames(contribution_shared), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Oxygen:Sulphur-d", "B"))
})



test_that("CreateContributionMatrix() produces a matrix of the correct format when basic_or_all_parameters = 'basic'", {
  data <- read.csv("Binary_long_cov_for_contribution_matrix.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Oxygen", "Sulphur"))
  
  contribution_unrelated <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "unrelated", study_or_arm_level = "arm", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage", weight_or_contribution = "weight")
  
  contribution_shared <- CreateContributionMatrix(data = data, treatment_ids = treatment_ids, outcome_type = "Binary", outcome_measure = "OR", effects_type = "fixed", cov_parameters = "shared", study_or_arm_level = "arm", basic_or_all_parameters = "basic", absolute_or_percentage = "percentage", weight_or_contribution = "weight")
  
  expect_equal(rownames(contribution_unrelated), c("(A)Hydrogen", "(A)Oxygen", "(B)Hydrogen", "(B)Oxygen", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Hydrogen", "(D)Sulphur"))
  expect_equal(colnames(contribution_unrelated), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "Hydrogen:Oxygen-beta", "Hydrogen:Sulphur-beta"))
  
  expect_equal(rownames(contribution_shared), c("(A)Hydrogen", "(A)Oxygen", "(B)Hydrogen", "(B)Oxygen", "(C)Hydrogen", "(C)Oxygen", "(C)Sulphur", "(D)Hydrogen", "(D)Sulphur"))
  expect_equal(colnames(contribution_shared), c("A-eta", "B-eta", "C-eta", "D-eta", "Hydrogen:Oxygen-d", "Hydrogen:Sulphur-d", "B"))
})



test_that("CalculateContributions() gathers covariate values for studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Treatment Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  expected_covariate_values <- unique(data[[covariate_title]])
  names(expected_covariate_values) <- studies
  
  covariate_values <- contributions$covariate_value
  names(covariate_values) <- studies
  expect_equal(
    !!covariate_values,
    !!expected_covariate_values
  )
})



test_that("CalculateContributions() gathers direct treatment effect contributions for treatments and studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Treatment Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_direct_contributions <- matrix(
    data = c(
      1.1, NA,  NA,  NA,
      NA,  4.4, NA,  NA,
      NA,  NA,  5.5, 6.6,
      NA,  NA,  NA,  NA,
      NA,  NA,  9.9, NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_direct_contributions) <- studies
  colnames(expected_direct_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$direct,
    !!expected_direct_contributions
  )
})



test_that("CalculateContributions() gathers indirect treatment effect contributions for treatments and studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Treatment Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_indirect_contributions <- matrix(
    data = c(
      NA,  2.2, NA,  NA,
      3.3, NA,  NA,  NA,
      NA,  NA,  NA,  NA,
      7.7, 8.8, NA,  NA,
      NA,  NA,  NA,  NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_indirect_contributions) <- studies
  colnames(expected_indirect_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$indirect,
    !!expected_indirect_contributions
  )
})



test_that("CalculateContributions() gathers direct covariate effect contributions for treatments and studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Covariate Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_direct_contributions <- matrix(
    data = c(
      11, NA, NA, NA,
      NA, 22, NA, NA,
      NA, NA, 33, 33,
      NA, NA, NA, NA,
      NA, NA, 55, NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_direct_contributions) <- studies
  colnames(expected_direct_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$direct,
    !!expected_direct_contributions
  )
})



test_that("CalculateContributions() gathers indirect covariate effect contributions for treatments and studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Covariate Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  # All studies contribute to all treatments regardless of network configuration
  # Therefore all studies which do not contribute directly, contribute indirectly
  expected_indirect_contributions <- matrix(
    data = c(
      NA, 11, 11, 11,
      22, NA, 22, 22,
      33, 33, NA, NA,
      44, 44, 44, 44,
      55, 55, NA, 55
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_indirect_contributions) <- studies
  colnames(expected_indirect_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$indirect,
    !!expected_indirect_contributions
  )
})



test_that("CalculateContributions() gathers relative treatment effects for treatments and studies", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Treatment Effect", cov_parameters = "shared")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_relative_effects <- matrix(
    data = c(
      -11,  NA,  NA,  NA,
       NA, -22,  NA,  NA,
       NA,  NA, -33, -55,
       NA,  NA,  NA,  NA,
       NA,  NA, -77,  NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_relative_effects) <- studies
  colnames(expected_relative_effects) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$relative_effect,
    !!expected_relative_effects
  )
})



test_that("CalculateContributions() gathers direct covariate effect contributions for treatments and studies for unrelated regression", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Covariate Effect", cov_parameters = "unrelated")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_direct_contributions <- matrix(
    data = c(
      11, NA, NA, NA,
      NA, 44, NA, NA,
      NA, NA, 55, 66,
      NA, NA, NA, NA,
      NA, NA, 99, NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_direct_contributions) <- studies
  colnames(expected_direct_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$direct,
    !!expected_direct_contributions
  )
})



test_that("CalculateContributions() gathers indirect covariate effect contributions for treatments and studies for unrelated regression", {
  setup <- SetupAndCalculateContributionMatrix(treatment_or_covariate_effect = "Covariate Effect", cov_parameters = "unrelated")
  data <- setup$data
  treatment_ids <- setup$treatment_ids
  covariate_title <- setup$covariate_title
  contributions <- setup$contributions
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_indirect_contributions <- matrix(
    data = c(
      NA, 22, NA, NA,
      33, NA, NA, NA,
      NA, NA, NA, NA,
      77, 88, NA, NA,
      NA, NA, NA, NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_indirect_contributions) <- studies
  colnames(expected_indirect_contributions) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$indirect,
    !!expected_indirect_contributions
  )
})



test_that("CreateContributionMatrix() produces correct output for the Donegan example", {
  #Load data
  donegan_original <- read.csv("Donegan_contribution_matrix_data.csv")
  donegan <- read.csv("Donegan_for_MetaInsight.csv")
  donegan_ids <- data.frame(Number = 1:3, Label = as.character(1:3))
  donegan_studies <- unique(donegan$Study)

  #The donegan dataset is in contrast form, which is not accepted by MetaInsight at the time of writing. The functions CreateVMatrix() and GetOutcomesAndVariances() require arm-level data, whereas all other functions do not. Therefore to reproduce the contribution matrix given in the publication, these two functions must be overwritten to simulate what would have been produced had the data been arm-level.
  
  #Mocked output from CreateVMatrix(), because the example is contrast-level rather than arm-level
  mocked_v <- diag(rep(donegan_original$se^2, each = 2))
  #Create dummy arms, since the dataset does not have arms
  dummy_arms <- paste0("Treat", rep(1:2, times = length(donegan_original$s)))
  rownames(mocked_v) <- paste0("(", rep(donegan_studies, each = 2), ")",  dummy_arms)
  colnames(mocked_v) <- rownames(mocked_v)
  
  #Mocked output from GetOutcomesAndVariances(), because the example is contrast-level rather than arm-level
  #Create dummy control arms with outcome equal to 0
  dummy_control_outcomes <- rep(0, length = length(donegan_original$lor))
  #Since the dummy outcomes in the control arm are 0, the effect sizes are the outcomes in the intervention arm
  #A trick using as.vector and rbind to intersperse two vectors, alternating between the elements
  mocked_outcomes <- data.frame(Outcome = as.vector(rbind(dummy_control_outcomes, donegan_original$lor)))
  
  mockery::stub(CreateContributionMatrix, "CreateVMatrix", mocked_v)
  mockery::stub(CreateContributionMatrix, "GetOutcomesAndVariances", mocked_outcomes)
  
  contribution_matrix_full <- CreateContributionMatrix(data = donegan,
                                                       treatment_ids = donegan_ids,
                                                       outcome_type = "Binary", 
                                                       outcome_measure = "OR",
                                                       effects_type = "fixed",
                                                        cov_parameters = "unrelated",
                                                       basic_or_all_parameters = "all",
                                                       study_or_arm_level = "study",
                                                       absolute_or_percentage = "percentage",
                                                       weight_or_contribution = "weight",
                                                       full_output = FALSE) |> round(digits = 2)
  #Drop the study-specific intercepts
  contribution_matrix <- contribution_matrix_full[, 25:30]
  
  expected_contribution_matrix <- read.csv("Donegan_contribution_matrix_expected.csv")
  expected_contribution_matrix <- as.matrix(expected_contribution_matrix[, 2:7])
  #Column and row names have already been tested in earlier tests, so no need to check here.
  colnames(expected_contribution_matrix) <- c("1:2-d", "1:3-d", "2:3-d", "1:2-beta", "1:3-beta", "2:3-beta")
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight") |> round(digits = 2)
  
  expected_V <- diag(c(2^2/50, 2^2/51, 3^2/60, 3^2/61, 3^2/62))
  expected_X <- matrix(c(1, 0, 0, 0, 0,
                         1, 0, 1, 0, 1-1.25,
                         0, 1, 0, 0, 0,
                         0, 1, 1, 0, 1.5-1.25,
                         0, 1, 0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 5)
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight") |> round(digits = 2)
  
  expected_V <- diag(c(2^2/50, 2^2/51, 3^2/60, 3^2/61, 3^2/62))
  expected_lambda_beta <- diag(0.9^2, nrow = 2)
  expected_V_star <- as.matrix(bdiag(expected_V, expected_lambda_beta))
  expected_X_d <- matrix(c(1, 0, 0, 0,
                           1, 0, 1, 0,
                           0, 1, 0, 0,
                           0, 1, 1, 0,
                           0, 1, 0, 1),
                         byrow = TRUE, nrow = 5)
  expected_X_beta <- matrix(c(       0,        0,
                                1-1.25,        0,
                                     0,        0,
                              1.5-1.25,        0,
                                     0, 1.5-1.25),
                            byrow = TRUE, nrow = 5)
  expected_X_star <- rbind(cbind(expected_X_d, expected_X_beta, matrix(0, nrow = 5, ncol = 1)),
                           cbind(matrix(0, nrow = 2, ncol = 4), diag(1, nrow = 2), matrix(1, nrow = 2, ncol = 1)))
  expected_XVX_matrix <- solve(t(expected_X_star)%*% solve(expected_V_star) %*% expected_X_star) %*% t(expected_X_star) %*% solve(expected_V_star)
  expected_A_matrix <- expected_XVX_matrix[1:6, 1:5]
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  expected_V <- diag(c(2^2/50, 2^2/51, 3^2/60, 3^2/61, 3^2/62))
  expected_lambda_tau <- diag(0.9^2, nrow = 5)
  expected_V_star <- as.matrix(bdiag(expected_V, expected_lambda_tau))
  expected_X <- matrix(c(1, 0, 0, 0, 0,
                         1, 0, 1, 0, 1-1.25,
                         0, 1, 0, 0, 0,
                         0, 1, 1, 0, 1.5-1.25,
                         0, 1, 0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 5)
  expected_X_star <- rbind(cbind(diag(1, nrow = 5), matrix(0, nrow = 5, ncol = 5)),
                           cbind(diag(1, nrow = 5), -expected_X))
  expected_XVX_matrix <- solve(t(expected_X_star)%*% solve(expected_V_star) %*% expected_X_star) %*% t(expected_X_star) %*% solve(expected_V_star)
  expected_A_matrix <- expected_XVX_matrix[6:10, 1:5]
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight")
  
  expected_V <- diag(c(2^2/50, 2^2/51, 3^2/60, 3^2/61, 3^2/62))
  expected_X <- matrix(c(1, 0, 0, 0, 0,
                         1, 0, 1, 0, 1-1.25,
                         0, 1, 0, 0, 0,
                         0, 1, 1, 0, 1.5-1.25,
                         0, 1, 0, 1, 1.5-1.25),
                       byrow = TRUE, nrow = 5)
  expected_Z_top_left <- diag(1, nrow = 2)
  expected_Z_middle <- matrix(c( 1, 0,
                                 0, 1,
                                -1, 1),
                              nrow = 3, ncol = 2, byrow = TRUE)
  expected_Z_bottom_right <- matrix(1)
  expected_Z <- as.matrix(bdiag(expected_Z_top_left,
                                expected_Z_middle,
                                expected_Z_bottom_right)
                          )
  rownames(expected_Z) <- c("A-eta", "B-eta", "Hydrogen:Neon-d", "Hydrogen:Carbon-d", "Neon:Carbon-d", "B")
  colnames(expected_Z) <- c("A-eta", "B-eta", "Hydrogen:Neon-d", "Hydrogen:Carbon-d", "B")
  expected_XVX_matrix <- solve(t(expected_X)%*% solve(expected_V) %*% expected_X) %*% t(expected_X) %*% solve(expected_V)
  expected_contribution_matrix <- round(t(abs(expected_Z %*% expected_XVX_matrix)), digits = 2)
  rownames(expected_contribution_matrix) <- rownames(contribution_matrix)
  colnames(expected_contribution_matrix) <- colnames(contribution_matrix)
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})



test_that("CreateContributionMatrix() adds up rows correctly when 'study_or_arm_level' = 'study'", {
  data <- read.csv("Cont_long_cont_cov_small.csv")
  
  treatment_ids <- list(Number = 1:3, Label = c("Hydrogen", "Neon", "Carbon"))
  
  contribution_matrix <- CreateContributionMatrix(data = data,
                                                  treatment_ids = treatment_ids,
                                                  outcome_type = "Continuous",                           
                                                  outcome_measure = "MD",
                                                  effects_type = "fixed",
                                                  cov_parameters = "shared",
                                                  basic_or_all_parameters = "basic",
                                                  study_or_arm_level = "study",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "weight") |> round(digits = 2)
  
  comparison_matrix <- CreateContributionMatrix(data = data,
                                                treatment_ids = treatment_ids,
                                                outcome_type = "Continuous",                           
                                                outcome_measure = "MD",
                                                effects_type = "fixed",
                                                cov_parameters = "shared",
                                                basic_or_all_parameters = "basic",
                                                study_or_arm_level = "arm",
                                                absolute_or_percentage = "absolute",
                                                weight_or_contribution = "weight") |> round(digits = 2)
  
  expected_contribution_matrix <- rbind(colSums(comparison_matrix[1:2, ]),
                                        colSums(comparison_matrix[3:5, ]))
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "percentage",
                                                  weight_or_contribution = "weight")
  
  absolute_contribution_matrix <- CreateContributionMatrix(data = data,
                                                           treatment_ids = treatment_ids,
                                                           outcome_type = "Continuous",                           
                                                           outcome_measure = "MD",
                                                           effects_type = "fixed",
                                                           cov_parameters = "shared",
                                                           basic_or_all_parameters = "basic",
                                                           study_or_arm_level = "arm",
                                                           absolute_or_percentage = "absolute",
                                                           weight_or_contribution = "weight")
  
  column_totals <- colSums(absolute_contribution_matrix)
  column_totals_matrix <- matrix(rep(column_totals, times = 5), byrow = TRUE, nrow = 5)
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
                                                  study_or_arm_level = "arm",
                                                  absolute_or_percentage = "absolute",
                                                  weight_or_contribution = "contribution")
  
  weight_matrix <- CreateContributionMatrix(data = data,
                                            treatment_ids = treatment_ids,
                                            outcome_type = "Continuous",                           
                                            outcome_measure = "MD",
                                            effects_type = "fixed",
                                            cov_parameters = "shared",
                                            basic_or_all_parameters = "basic",
                                            study_or_arm_level = "arm",
                                            absolute_or_percentage = "absolute",
                                            weight_or_contribution = "weight")
  
  outcomes <- data$Mean
  outcomes_matrix <- matrix(rep(outcomes, times = 5), nrow = 5)
  expected_contribution_matrix <- weight_matrix * outcomes_matrix
  
  expect_equal(contribution_matrix, expected_contribution_matrix)
})
