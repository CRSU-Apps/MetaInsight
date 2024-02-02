test_that("FormatForBnma() gives correct data for wide binary", {
  
  # process data as would be in app
  data <- read.csv("Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  bnma_data <- FormatForBnma(br_data=data,
                             treatment_ids=wrangled_treatment_list,
                             outcome_type="Binary",
                             ref="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = 30:37,
    N = 100:107)
  
  #The expected order is the reference first, followed by the rest in the order they appear in the long version of the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(WideToLong(data, outcome_type="Binary")$T)], first_item = "the_Little")
  
  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for long binary", {
  
  data <- read.csv("Binary_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  bnma_data <- FormatForBnma(br_data=data,
                             treatment_ids=wrangled_treatment_list,
                             outcome_type="Binary",
                             ref="the_Little")

  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = 30:37,
    N = 100:107)

  #The expected order is the reference first, followed by the rest in the order they appear in the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(data$T)], first_item = "the_Little")
  
  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for wide continuous", {
  
  data <- read.csv("Cont_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  bnma_data <- FormatForBnma(br_data=data,
                             treatment_ids=wrangled_treatment_list,
                             outcome_type="Continuous",
                             ref="the_Little")
  
  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
    SD = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    N = 30:37)
  
  #The expected order is the reference first, followed by the rest in the order they appear in the long version of the data
  expected_Treat.order <-  VectorWithItemFirst(vector = wrangled_treatment_list$Label[unique(WideToLong(data, outcome_type="Binary")$T)], first_item = "the_Little")
  
  expect_equal(bnma_data$ArmLevel, expected_ArmLevel)
  expect_equal(bnma_data$Treat.order, expected_Treat.order)
})



test_that("FormatForBnma() gives correct data for long continuous", {
  
  data <- read.csv("Cont_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  bnma_data <- FormatForBnma(br_data=data,
                             treatment_ids=wrangled_treatment_list,
                             outcome_type="Continuous",
                             ref="the_Little")
  
  expected_ArmLevel <- data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    Treat = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    Outcomes = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
    SD = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    N = 30:37)

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
                                      outcome_type = "Binary",
                                      effects_type = "random",
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
                                      outcome_type = "Binary",
                                      effects_type = "random",
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
                                      outcome_type = "Continuous",
                                      effects_type = "fixed",
                                      cov_parameters = "exchangeable")
    
  expect_equal(bnma_network$response, "normal")
})



test_that("BaselineRiskRegression() sets RNGs correctly and gives reproducible output", {
  
  data <- list(ArmLevel = data.frame(
    Study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    T = c(1, 2, 3, 4, 1, 5, 1, 6),
    Mean = c(-1, -2.1, -3.2, -4.3, -1.4, -5.5, -1.6, -7.7),
    SD = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    N = 30:37))
  data$Treat.order <- VectorWithItemFirst(vector = unique(data$ArmLevel$Treat), first_item = "the_Great")
  
  treatment_ids <- data.frame(Number = 1:6, Label = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Butcher", "the_Slit_nosed"))

  result_1 <- BaselineRiskRegression(br_data = data$ArmLevel,
                                     treatment_ids = treatment_ids,
                                     outcome_type = "Continuous",
                                     ref = "the_Great",
                                     effects_type = "random",
                                     cov_parameters = "exchangeable",
                                     seed = 97531)
  
  result_2 <- BaselineRiskRegression(br_data = data$ArmLevel,
                                     treatment_ids = treatment_ids,
                                     outcome_type = "Continuous",
                                     ref = "the_Great",
                                     effects_type = "random",
                                     cov_parameters = "exchangeable",
                                     seed = 97531)
  
  expect_equal(result_1$inits[[1]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[2]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[3]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$inits[[4]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(result_1$samples[1], result_2$samples[1])
  expect_equal(result_1$samples[2], result_2$samples[2])
  expect_equal(result_1$samples[3], result_2$samples[3])
  expect_equal(result_1$samples[4], result_2$samples[4])
  
})
