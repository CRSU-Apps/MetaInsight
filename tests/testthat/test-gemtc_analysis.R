test_that("PrepDataGemtc() gives correct data for wide binary", {
  
  data <- read.csv("Binary_wide_cov.csv")
  gemtc_data <- PrepDataGemtc(data, "Binary", "covar.age", "age")
  
  expected_data <- list(
    armData = data.frame(
      study=c(rep("Constantine",3), rep("Leo",3), rep("Justinian",2)),
      treatment=c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
      responders=30:37,
      sampleSize=100:107),
    studyData = data.frame(
      study=c("Constantine", "Leo", "Justinian"),
      age=rep(99,3))
  )
  
  expect_equal(gemtc_data, expected_data)
})

test_that("PrepDataGemtc() gives correct data for long binary", {
  
  data <- read.csv("Binary_long_cov.csv")
  gemtc_data <- PrepDataGemtc(data, "Binary", "covar.age", "age")
  
  expected_data <- list(
    armData = data.frame(
      study=c(rep("Constantine",3), rep("Leo",3), rep("Justinian",2)),
      treatment=c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
      responders=30:37,
      sampleSize=100:107),
    studyData = data.frame(
      study=c("Constantine", "Leo", "Justinian"),
      age=rep(99,3))
  )
  
  expect_equal(gemtc_data, expected_data)
})

test_that("PrepDataGemtc() gives correct data for wide continuous", {
  
  data <- read.csv("Cont_wide_cov.csv")
  gemtc_data <- PrepDataGemtc(data, "Continuous", "covar.age", "age")
  
  expected_data <- list(
    armData = data.frame(
      study=c(rep("Constantine",3), rep("Leo",3), rep("Justinian",2)),
      treatment=c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
      mean=c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
      std.dev=c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
      sampleSize=30:37),
    studyData = data.frame(
      study=c("Constantine", "Leo", "Justinian"),
      age=rep(99,3))
  )
  
  expect_equal(gemtc_data, expected_data)
})

test_that("PrepDataGemtc() gives correct data for long continuous", {
  
  data <- read.csv("Cont_long_cov.csv")
  gemtc_data <- PrepDataGemtc(data, "Continuous", "covar.age", "age")
  
  expected_data <- list(
    armData = data.frame(
      study=c(rep("Constantine",3), rep("Leo",3), rep("Justinian",2)),
      treatment=c("the Great", "the Younger", "the Dung-named", "the Little", "the Great", "the Butcher", "the Great", "the Slit-nosed"),
      mean=c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
      std.dev=c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
      sampleSize=30:37),
    studyData = data.frame(
      study=c("Constantine", "Leo", "Justinian"),
      age=rep(99,3))
  )
  
  expect_equal(gemtc_data, expected_data)
})