library(mockery)

#' Stub the functions `CreateContributionMatrix()` and `frequentist()`, and run the contribution matrix calculation.
#' @param treatment_or_covariate_effect Whether contributions are for treatment effect or covariate effect. One of: "Treatment Effect", "Covariate Effect".
#' @param regression_coefficient_type Type of regression coefficient. One of: "Shared", "Unrelated", "Exchangeable"
#'
#' @return List of objects used in the test:
#' - data
#'   - Data frame being analysed
#' - treatment_ids
#'   - Data frame of treatment names ("Label") and IDs ("Number")
#' - covariate_title
#'   - Title of covariate column in data
#' - contributions
#'   - Output from function `CalculateContributions()`
SetupAndCalculateContributionMatrix <- function(treatment_or_covariate_effect, regression_coefficient_type) {
  covariate_title <- "covar.age"
  
  # Setup data
  data <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D", "E", "E"),
    T = c(1, 2, 1, 3, 1, 4, 5, 2, 3, 1, 4),
    covar.age = c(911, 911, 4, 4, 72, 72, 72, 46, 46, 63, 63)
  )
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Hydrogen", "Oxygen", "Sulphur", "Zinc", "Einsteinium")
  )
  
  reference_name <- treatment_ids$Label[1]
  
  studies <- unique(data$Study)
  non_reference_treatments <- treatment_ids$Label[treatment_ids$Label != reference_name]
  
  if (regression_coefficient_type == "Shared") {
    covariate_effect_columns <- "B"
  } else {
    covariate_effect_columns <- paste0(reference_name, ":", non_reference_treatments, "_beta")
  }
  
  # Setup mocked contribution matrix
  mock_contribution_matrix <- matrix(
    0,
    nrow = length(studies),
    ncol = length(non_reference_treatments) + length(covariate_effect_columns)
  )
  row.names(mock_contribution_matrix) <- studies
  colnames(mock_contribution_matrix) <- c(paste0(reference_name, ":", non_reference_treatments, "_d"), covariate_effect_columns)
  
  mock_contribution_matrix["A", "Hydrogen:Oxygen_d"] <- 1.1
  mock_contribution_matrix["A", "Hydrogen:Sulphur_d"] <- 2.2
  mock_contribution_matrix["B", "Hydrogen:Oxygen_d"] <- 3.3
  mock_contribution_matrix["B", "Hydrogen:Sulphur_d"] <- 4.4
  mock_contribution_matrix["C", "Hydrogen:Zinc_d"] <- 5.5
  mock_contribution_matrix["C", "Hydrogen:Einsteinium_d"] <- 6.6
  mock_contribution_matrix["D", "Hydrogen:Oxygen_d"] <- 7.7
  mock_contribution_matrix["D", "Hydrogen:Sulphur_d"] <- 8.8
  mock_contribution_matrix["E", "Hydrogen:Zinc_d"] <- 9.9
  
  if (regression_coefficient_type == "Shared") {
    mock_contribution_matrix["A", "B"] <- 11
    mock_contribution_matrix["B", "B"] <- 22
    mock_contribution_matrix["C", "B"] <- 33
    mock_contribution_matrix["D", "B"] <- 44
    mock_contribution_matrix["E", "B"] <- 55
  } else {
    mock_contribution_matrix["A", "Hydrogen:Oxygen_beta"] <- 11
    mock_contribution_matrix["A", "Hydrogen:Sulphur_beta"] <- 22
    mock_contribution_matrix["B", "Hydrogen:Oxygen_beta"] <- 33
    mock_contribution_matrix["B", "Hydrogen:Sulphur_beta"] <- 44
    mock_contribution_matrix["C", "Hydrogen:Zinc_beta"] <- 55
    mock_contribution_matrix["C", "Hydrogen:Einsteinium_beta"] <- 66
    mock_contribution_matrix["D", "Hydrogen:Oxygen_beta"] <- 77
    mock_contribution_matrix["D", "Hydrogen:Sulphur_beta"] <- 88
    mock_contribution_matrix["E", "Hydrogen:Zinc_beta"] <- 99
  }
  
  # Setup mocked frequentist analysis
  mock_frequentist_d0 <- data.frame(
    Study = c("A", "B", "C", "C", "C", "D", "E"),
    treat1 = c(1, 1, 1, 4, 5, 2, 1),
    treat2 = c(2, 3, 4, 5, 1, 3, 4),
    TE = c(11, 22, 33, 44, 55, 66, 77)
  )
  
  # Stub functions
  mockery::stub(CalculateContributions, "CreateContributionMatrix", mock_contribution_matrix)
  mockery::stub(CalculateContributions, "netmeta::pairwise", mock_frequentist_d0)
  
  # Run calculation
  # Unused parameters set to NULL
  contributions <- CalculateContributions(
    data = data,
    covariate_title = covariate_title,
    treatment_ids = treatment_ids,
    outcome_type = "Binary",
    outcome_measure = "OR",
    effects_type = "random",
    regression_coefficient_type = regression_coefficient_type,
    std_dev_d = NULL,
    cov_parameters,
    cov_centre = NULL,
    std_dev_beta = NULL,
    study_or_comparison_level = NULL,
    absolute_or_percentage = NULL,
    basic_or_all_parameters = NULL,
    weight_or_contribution = NULL,
    treatment_or_covariate_effect = treatment_or_covariate_effect
  )
  
  return(
    list(
      data = data,
      treatment_ids = treatment_ids,
      covariate_title = covariate_title,
      contributions = contributions
    )
  )
}
