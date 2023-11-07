test_that("PrepDataGemtc() gives correct data for wide binary", {
  
  # process data as would be in app
  data <- read.csv("Binary_wide_continuous_cov.csv")
  treatment_ids <- CleanTreatmentIds(CreateTreatmentIds(FindAllTreatments(data)))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  gemtc_data <- PrepDataGemtc(data, treatment_ids, "Binary", "covar.age", "age")
  
  expected_armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
    responders = 30:37,
    sampleSize = 100:107)
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  
  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for long binary", {
  
  data <- read.csv("Binary_long_continuous_cov.csv")
  treatment_ids <- CleanTreatmentIds(CreateTreatmentIds(FindAllTreatments(data)))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  gemtc_data <- PrepDataGemtc(data, treatment_ids, "Binary", "covar.age", "age")
  
  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
    responders = 30:37,
    sampleSize = 100:107)
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  
  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for wide continuous", {
  
  data <- read.csv("Cont_wide_continuous_cov.csv")
  treatment_ids <- CleanTreatmentIds(CreateTreatmentIds(FindAllTreatments(data)))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  
  gemtc_data <- PrepDataGemtc(data, treatment_ids, "Continuous", "covar.age", "age")
  
  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
    mean = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
    std.dev = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    sampleSize = 30:37)
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  
  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for long continuous", {
  
  data <- read.csv("Cont_long_continuous_cov.csv")
  treatment_ids <- CleanTreatmentIds(CreateTreatmentIds(FindAllTreatments(data)))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  
  gemtc_data <- PrepDataGemtc(data, treatment_ids, "Continuous", "covar.age", "age")
  
  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
    mean = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
    std.dev = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
    sampleSize = 30:37)
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  
  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("CreateGemtcModel() assigns model type, covariate type, reference treatment, and RNGs correctly", {
  
  data <- list(armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = 30:37,
    sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'OR', 'unrelated', 'the_Great')
  
  expect_equal(gemtc_model$linearModel, "random")
  expect_equal(gemtc_model$regressor$coefficient, "unrelated")
  expect_equal(as.character(gemtc_model$regressor$control), "the_Great")
  expect_equal(gemtc_model$inits[[1]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(gemtc_model$inits[[2]]$.RNG.name, "base::Marsaglia-Multicarry")
  expect_equal(gemtc_model$inits[[3]]$.RNG.name, "base::Super-Duper")
  expect_equal(gemtc_model$inits[[4]]$.RNG.name, "base::Mersenne-Twister")
  
})

test_that("CreateGemtcModel() has correct model settings for OR outcome", {
  
  data <- list(armData = data.frame(
      study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      responders = 30:37,
      sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'OR', 'shared', 'the_Great')
  
  expect_equal(gemtc_model$likelihood, "binomial")
  expect_equal(gemtc_model$link, "logit")
  
})

test_that("CreateGemtcModel() has correct model settings for RR outcome", {
  
  data <- list(armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = 30:37,
    sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'RR', 'shared', 'the_Great')
  
  expect_equal(gemtc_model$likelihood, "binomial")
  expect_equal(gemtc_model$link, "log")
  
})

test_that("CreateGemtcModel() has correct model settings for MD outcome", {
  
  data <- list(armData = data.frame(
      study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      mean = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
      std.dev = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
      sampleSize = 30:37),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'MD', 'shared', 'the_Great')
  
  expect_equal(gemtc_model$likelihood, "normal")
  expect_equal(gemtc_model$link, "identity")
  
})

test_that("RunCovariateModel() gives reproducible output", {
  
  data <- read.csv("Binary_wide_continuous_cov.csv")
  treatment_ids <- CleanTreatmentIds(CreateTreatmentIds(FindAllTreatments(data)))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  result_1 <- RunCovariateModel(data, treatment_ids, "Binary", 'OR', "covar.age", "age", 'random', 'unrelated', ref_choice)
  result_2 <- RunCovariateModel(data, treatment_ids, "Binary", 'OR', "covar.age", "age", 'random', 'unrelated', ref_choice)
  
  expect_equal(result_1$samples[1], result_2$samples[1])
  expect_equal(result_1$samples[2], result_2$samples[2])
  expect_equal(result_1$samples[3], result_2$samples[3])
  expect_equal(result_1$samples[4], result_2$samples[4])
  
})
