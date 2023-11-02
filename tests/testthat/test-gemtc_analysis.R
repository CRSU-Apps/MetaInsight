test_that("PrepDataGemtc() gives correct data for wide binary", {
  
  data <- read.csv("Binary_wide_continuous_cov.csv")
  gemtc_data <- PrepDataGemtc(data, "Binary", "covar.age", "age")
  
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
  gemtc_data <- PrepDataGemtc(data, "Binary", "covar.age", "age")
  
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
  gemtc_data <- PrepDataGemtc(data, "Continuous", "covar.age", "age")
  
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
  gemtc_data <- PrepDataGemtc(data, "Continuous", "covar.age", "age")
  
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

test_that("RunCovariateRegression() gives a mtc.result object for binary analysis", {
  
  data <- list(armData = data.frame(
      study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      responders = 30:37,
      sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  
  result <- RunCovariateRegression(data, "random", "shared", "the_Great")
  
  expect_s3_class(result, "mtc.result")
  
})

test_that("RunCovariateRegression() gives reproducible output", {
  
  data <- list(armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = 30:37,
    sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  
  result_1 <- RunCovariateRegression(data, "random", "shared", "the_Great")
  result_2 <- RunCovariateRegression(data, "random", "shared", "the_Great")
  
  expect_equal(result_1$samples[1], result_2$samples[1])
  expect_equal(result_1$samples[2], result_2$samples[2])
  expect_equal(result_1$samples[3], result_2$samples[3])
  expect_equal(result_1$samples[4], result_2$samples[4])
  
})