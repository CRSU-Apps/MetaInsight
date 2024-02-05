
test_that("ValidateUploadedData() identifies valid continuous long data", {
  data <- CleanData(read.csv("Cont_long.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid continuous wide data", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid binary long data", {
  data <- CleanData(read.csv("Binary_long.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies valid binary wide data", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_true(validation_result$valid)
  expect_equal(validation_result$message, "Data is valid")
})

test_that("ValidateUploadedData() identifies invalid continuous long data, missing colunm 'N'", {
  data <- CleanData(read.csv("invalid_data/continuous-missing-long.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for Continuous data: N")
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, missing all colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/continuous-missing-entirely-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for Continuous data: N")
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, missing some colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/continuous-missing-partially-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid continuous wide data, with inconsistently numbered colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/continuous-misnumbered-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Continuous")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, N, Mean, SD) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid binary long data, missing colunm 'N'", {
  data <- CleanData(read.csv("invalid_data/binary-missing-long.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for Binary data: N")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, missing all colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/binary-missing-entirely-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "Missing columns for Binary data: N")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, missing some colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/binary-missing-partially-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
})

test_that("ValidateUploadedData() identifies invalid binary wide data, with inconsistently numbered colunms 'N.*'", {
  data <- CleanData(read.csv("invalid_data/binary-misnumbered-wide.csv"))
  
  validation_result <- ValidateUploadedData(data, "Binary")
  
  expect_false(validation_result$valid)
  expect_equal(validation_result$message, "For wide format data, numbered columns (T, R, N) must all have matching sequential indices, starting from 1")
})
