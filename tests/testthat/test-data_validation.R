#
# test_that("ValidateUploadedData() identifies valid continuous long data", {
#   data <- CleanData(read.csv("data/Cont_long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_true(validation_result$valid)
#   expect_equal(validation_result$message, "Data is valid")
# })
#
# test_that("ValidateUploadedData() identifies valid continuous wide data", {
#   data <- CleanData(read.csv("data/Cont_wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_true(validation_result$valid)
#   expect_equal(validation_result$message, "Data is valid")
# })
#
# test_that("ValidateUploadedData() identifies valid binary long data", {
#   data <- CleanData(read.csv("data/Binary_long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_true(validation_result$valid)
#   expect_equal(validation_result$message, "Data is valid")
# })
#
# test_that("ValidateUploadedData() identifies valid binary wide data", {
#   data <- CleanData(read.csv("data/Binary_wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_true(validation_result$valid)
#   expect_equal(validation_result$message, "Data is valid")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous empty file", {
#   validation_result <- ValidateUploadedData(NULL, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary empty file", {
#   validation_result <- ValidateUploadedData(NULL, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous long empty file", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-empty-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous wide empty file", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-empty-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary long empty file", {
#   data <- CleanData(read.csv("data/invalid_data/binary-empty-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide empty file", {
#   data <- CleanData(read.csv("data/invalid_data/binary-empty-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "File is empty")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous long data, missing colunm 'N'", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-missing-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Missing columns for Continuous data: N")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous long data, with mistyped 'Study' and 'Mean' columns", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-mistyped-columns-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(
#     validation_result$message,
#     "Some columns have incorrect data types: Study should be of type character (text), Mean should be of type numeric"
#   )
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous wide data, with mistyped 'Study' and 'Mean.1' columns", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-mistyped-columns-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(
#     validation_result$message,
#     "Some columns have incorrect data types: Study should be of type character (text), Mean.1 should be of type numeric"
#   )
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous wide data, missing all colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-missing-entirely-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Missing columns for Continuous data: N")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous wide data, missing some colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-missing-partially-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
# })
#
# test_that("ValidateUploadedData() identifies invalid continuous wide data, with inconsistently numbered colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-misnumbered-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary long data, missing colunm 'N'", {
#   data <- CleanData(read.csv("data/invalid_data/binary-missing-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Missing columns for Binary data: N")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary long data, with mistyped 'Study' and 'R' columns", {
#   data <- CleanData(read.csv("data/invalid_data/binary-mistyped-columns-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(
#     validation_result$message,
#     "Some columns have incorrect data types: Study should be of type character (text), R should be of type numeric"
#   )
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, with mistyped 'Study' and 'R.3' columns", {
#   data <- CleanData(read.csv("data/invalid_data/binary-mistyped-columns-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(
#     validation_result$message,
#     "Some columns have incorrect data types: Study should be of type character (text), R.3 should be of type numeric"
#   )
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, missing all colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/binary-missing-entirely-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Missing columns for Binary data: N")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, missing some colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/binary-missing-partially-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, with inconsistently numbered colunms 'N.*'", {
#   data <- CleanData(read.csv("data/invalid_data/binary-misnumbered-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, with single-arm studies", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-single-arm-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary long data, with single-arm studies", {
#   data <- CleanData(read.csv("data/invalid_data/continuous-single-arm-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Continuous")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary wide data, with single-arm studies", {
#   data <- CleanData(read.csv("data/invalid_data/binary-single-arm-wide.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
# })
#
# test_that("ValidateUploadedData() identifies invalid binary long data, with single-arm studies", {
#   data <- CleanData(read.csv("data/invalid_data/binary-single-arm-long.csv"))
#
#   validation_result <- ValidateUploadedData(data, "Binary")
#
#   expect_false(validation_result$valid)
#   expect_equal(validation_result$message, "Some studies have single arms: Constantine, Justinian")
# })


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

