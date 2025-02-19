
test_that("CalculateDirectness() gathers covariate values for studies", {
  data <- read.csv("data\\Test_directness.csv")
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- ReplaceTreatmentIds(data, treatment_ids)

  contributions <- CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome_type = "Binary",
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
  data <- read.csv("data\\Test_directness.csv")
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- ReplaceTreatmentIds(data, treatment_ids)
  
  contributions <- CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome_type = "Binary",
    outcome_measure = "OR",
    effects_type = "random"
  )
  
  studies <- unique(data$Study)
  
  # Numbers manually taken from data set
  expected_directness <- matrix(
    data = c(
      TRUE, FALSE, NA,
      TRUE, TRUE, NA,
      FALSE, TRUE, NA,
      NA, NA, TRUE,
      FALSE, FALSE, NA
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE
  )
  row.names(expected_directness) <- studies
  colnames(expected_directness) <- treatment_ids$Label[-1]
  
  expect_equal(
    !!contributions$direct,
    !!expected_directness
  )
})



test_that("CalculateContributions() gathers relative treatment effects for treatments and studies", {
  data <- read.csv("data\\Test_directness.csv")
  treatment_ids <- data.frame(
    Number = 1:4,
    Label = c("A", "B", "C", "D")
  )
  data <- ReplaceTreatmentIds(data, treatment_ids)
  
  contributions <- CalculateDirectness(
    data = data,
    covariate_title = "covar.age",
    treatment_ids = treatment_ids,
    outcome_type = "Binary",
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


