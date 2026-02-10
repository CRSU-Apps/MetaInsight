
test_that("ValidateUploadedData() identifies valid continuous long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid continuous wide data", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid binary long data", {
  data <- CleanData(read.csv("data/Binary_long.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid binary wide data", {
  data <- CleanData(read.csv("data/Binary_wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies invalid continuous empty file", {
  validation_result <- ValidateUploadedData(NULL, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid binary empty file", {
  validation_result <- ValidateUploadedData(NULL, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid continuous long empty file", {
  data <- CleanData(read.csv("data/invalid_data/continuous-empty-long.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid continuous wide empty file", {
  data <- CleanData(read.csv("data/invalid_data/continuous-empty-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid binary long empty file", {
  data <- CleanData(read.csv("data/invalid_data/binary-empty-long.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid binary wide empty file", {
  data <- CleanData(read.csv("data/invalid_data/binary-empty-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "File is empty")
})

test_that("ValidateUploadedData() identifies invalid continuous long data, missing colunm 'N'", {
  data <- CleanData(read.csv("data/invalid_data/continuous-missing-long.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for continuous data: N")
})

test_that("ValidateUploadedData() identifies invalid continuous long data, with mistyped 'Study' and 'Mean' columns", {
  data <- CleanData(read.csv("data/invalid_data/continuous-mistyped-columns-long.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(
    validation_result$message,
    "Some columns have incorrect data types: Study should be of type character (text), Mean should be of type numeric"
  )
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, with mistyped 'Study' and 'Mean.1' columns", {
  data <- CleanData(read.csv("data/invalid_data/continuous-mistyped-columns-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(
    validation_result$message,
    "Some columns have incorrect data types: Study should be of type character (text), Mean.1 should be of type numeric"
  )
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, missing all colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/continuous-missing-entirely-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for continuous data: N")
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, missing some colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/continuous-missing-partially-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, with inconsistently numbered colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/continuous-misnumbered-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid binary long data, missing colunm 'N'", {
  data <- CleanData(read.csv("data/invalid_data/binary-missing-long.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for binary data: N")
})

test_that("ValidateUploadedData() identifies invalid binary long data, with mistyped 'Study' and 'R' columns", {
  data <- CleanData(read.csv("data/invalid_data/binary-mistyped-columns-long.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(
    validation_result$message,
    "Some columns have incorrect data types: Study should be of type character (text), R should be of type numeric"
  )
})

test_that("ValidateUploadedData() identifies invalid binary wide data, with mistyped 'Study' and 'R.3' columns", {
  data <- CleanData(read.csv("data/invalid_data/binary-mistyped-columns-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(
    validation_result$message,
    "Some columns have incorrect data types: Study should be of type character (text), R.3 should be of type numeric"
  )
})

test_that("ValidateUploadedData() identifies invalid binary wide data, missing all colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/binary-missing-entirely-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for binary data: N")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, missing some colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/binary-missing-partially-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, with inconsistently numbered colunms 'N.*'", {
  data <- CleanData(read.csv("data/invalid_data/binary-misnumbered-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, with single-arm studies", {
  data <- CleanData(read.csv("data/invalid_data/continuous-single-arm-wide.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
})

test_that("ValidateUploadedData() identifies invalid binary long data, with single-arm studies", {
  data <- CleanData(read.csv("data/invalid_data/continuous-single-arm-long.csv"))

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, with single-arm studies", {
  data <- CleanData(read.csv("data/invalid_data/binary-single-arm-wide.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
})

test_that("ValidateUploadedData() identifies invalid binary long data, with single-arm studies", {
  data <- CleanData(read.csv("data/invalid_data/binary-single-arm-long.csv"))

  validation_result <- ValidateUploadedData(data, "binary")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
})

test_that("ValidateUploadedData() allows correct quality assessment columns", {
  data <- read.csv("data/Cont_long.csv")
  data$rob <- c(1, 1, 1, 2, 2, 2, 3, 3)
  data$indirectness <- c(2, 2, 2, 3, 3, 3, 1, 1)
  data$rob.test <- c(3, 3, 3, 1, 1, 1, 2, 2)

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies unallowed quality assessment values", {
  data <- read.csv("data/Cont_long.csv")
  data2 <- data
  data3 <- data

  data$rob <- c(1, 1, 1, 2, 2, 2, 4, 4)
  data$indirectness <- 1
  data$rob.test <- 1
  data2$rob <- 1
  data2$indirectness <- c(1, 1, 1, 4, 4, 4, 2, 2)
  data2$rob.test <- 1
  data3$rob <- 1
  data3$indirectness <- 1
  data3$rob.test <- c(1, 1, 1, 4, 4, 4, 2, 2)

  validation_result <- ValidateUploadedData(data, "continuous")
  validation_result2 <- ValidateUploadedData(data2, "continuous")
  validation_result3 <- ValidateUploadedData(data3, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have values for rob that are not 1, 2 or 3: Justinian")
  expect_false(validation_result2$valid)
  expect_equal(validation_result2$message, "Some studies have values for indirectness that are not 1, 2 or 3: Leo")
  expect_false(validation_result3$valid)
  expect_equal(validation_result3$message, "Some studies have values for rob.test that are not 1, 2 or 3: Leo")
})

test_that("ValidateUploadedData() identifies studies with partially empty quality assessment values", {
  data <- read.csv("data/Cont_long.csv")
  data2 <- data
  data3 <- data

  data$rob <- c(1, 1, 1, 1, 1, 1, NA, NA)
  data$indirectness <- 1
  data$rob.test <- 1
  data2$rob <- 1
  data2$indirectness <- c(1, 1, 1, NA, NA, 1, 1, 1)
  data2$rob.test <- 1
  data3$rob <- 1
  data3$indirectness <- 1
  data3$rob.test <- c(NA, NA, 1, 1, 1, 1, 1, 1)

  validation_result <- ValidateUploadedData(data, "continuous")
  validation_result2 <- ValidateUploadedData(data2, "continuous")
  validation_result3 <- ValidateUploadedData(data3, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies have values for rob that are not 1, 2 or 3: Justinian")
  expect_false(validation_result2$valid)
  expect_equal(validation_result2$message, "Some studies have values for indirectness that are not 1, 2 or 3: Leo")
  expect_false(validation_result3$valid)
  expect_equal(validation_result3$message, "Some studies have values for rob.test that are not 1, 2 or 3: Constantine")
})

test_that("ValidateUploadedData() identifies studies without unique quality values", {
  data <- read.csv("data/Cont_long.csv")
  data2 <- data
  data3 <- data

  data$rob <- c(1, 1, 1, 2, 2, 2, 1, 3)
  data$indirectness <- 1
  data$rob.test  <- 1
  data2$rob <- 1
  data2$indirectness <- c(1, 1, 1, 1, 2, 2, 1, 1)
  data2$rob.test  <- 1
  data3$rob <- 1
  data3$indirectness <- 1
  data3$rob.test  <- c(1, 1, 2, 1, 1, 1, 1, 1)

  validation_result <- ValidateUploadedData(data, "continuous")
  validation_result2 <- ValidateUploadedData(data2, "continuous")
  validation_result3 <- ValidateUploadedData(data3, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies do not have the same rob value for every arm: Justinian.")
  expect_false(validation_result2$valid)
  expect_equal(validation_result2$message, "Some studies do not have the same indirectness value for every arm: Leo.")
  expect_false(validation_result3$valid)
  expect_equal(validation_result3$message, "Some studies do not have the same rob.test value for every arm: Constantine.")
})

test_that("ValidateUploadedData() identifies when more than 10 individual rob components are present", {
  data <- read.csv("data/Cont_long.csv")

  data$rob.test1  <- 1
  data$rob.test2  <- 1
  data$rob.test3  <- 1
  data$rob.test4  <- 1
  data$rob.test5  <- 1
  data$rob.test5  <- 1
  data$rob.test6  <- 1
  data$rob.test7  <- 1
  data$rob.test8  <- 1
  data$rob.test9  <- 1
  data$rob.test10  <- 1
  data$rob.test11  <- 1

  validation_result <- ValidateUploadedData(data, "continuous")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "A maximum of 10 individual risk of bias variables are allowed.")
})

test_that(".ValidateCovariate() generates error message when long data has multiple different covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 43, 9, 9, 77, 33.33333)
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies contain inconsistent covariate values between arms: Jeff et al, Frank")
})

test_that(".ValidateCovariate() generates error message when long data has missing covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 11, NA, NA, NA, NA)
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies do not define covariate values for all arms: Steve and Al, Frank")
})

test_that(".ValidateCovariate() generates error message when long data has missing covariate values for only some arms", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, NA, 1, NA, 45, 45)
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies do not define covariate values for all arms: Jeff et al, Steve and Al")
})

test_that(".ValidateCovariate() generates error message when wide data has missing covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(11, NA, NA)
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Some studies do not define covariate values for all arms: Steve and Al, Frank")
})

test_that(".ValidateCovariate() generates error message when long data has non-numerical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c("11", "11", 9, 9, "77", "77")
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "One or more covariate values are non-numerical.")
})

test_that(".ValidateCovariate() generates error message when wide data has non-numerical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c("11", 9, "77")
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "One or more covariate values are non-numerical.")
})

test_that(".ValidateCovariate() generates error message when long data has all identical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 11, 11, 11, 11, 11)
  )

  validation_result <- .ValidateCovariate(df, "covar.bananas")

  expect_false(validation_result$valid)

  expect_equal(validation_result$message, "Cannot analyse covariate with no variation.")

})

