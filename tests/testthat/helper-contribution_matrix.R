library(mockery)

#' Stub the functions `CreateContributionMatrix()` and `frequentist()`, and run the contribution matrix calculation.
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
SetupAndCalculateContributionMatrix <- function() {
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
  
  # Setup mocked contribution matrix
  mock_contribution_matrix <- matrix(
    rep(0, length(studies) * (length(non_reference_treatments) + 1)),
    length(studies),
    length(non_reference_treatments) + 1
  )
  row.names(mock_contribution_matrix) <- studies
  colnames(mock_contribution_matrix) <- c(paste0(reference_name, ":", non_reference_treatments, "_d"), "B")
  
  mock_contribution_matrix["A", "Hydrogen:Oxygen_d"] <- 1.1
  mock_contribution_matrix["A", "Hydrogen:Sulphur_d"] <- 2.2
  mock_contribution_matrix["B", "Hydrogen:Oxygen_d"] <- 3.3
  mock_contribution_matrix["B", "Hydrogen:Sulphur_d"] <- 4.4
  mock_contribution_matrix["C", "Hydrogen:Zinc_d"] <- 5.5
  mock_contribution_matrix["C", "Hydrogen:Einsteinium_d"] <- 6.6
  mock_contribution_matrix["D", "Hydrogen:Oxygen_d"] <- 7.7
  mock_contribution_matrix["D", "Hydrogen:Sulphur_d"] <- 8.8
  mock_contribution_matrix["E", "Hydrogen:Zinc_d"] <- 9.9
  
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
    std_dev_d = NULL,
    cov_parameters,
    cov_centre = NULL,
    std_dev_beta = NULL,
    study_or_comparison_level = NULL,
    absolute_or_percentage = NULL,
    basic_or_all_parameters = NULL,
    weight_or_contribution = NULL
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
