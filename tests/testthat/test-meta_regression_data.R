
test_that("InferCovariateType() throws error when long data has multiple different covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 43, 9, 9, 77, 33.33333)
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "Some studies contain inconsistent covariate values: Jeff et al, Frank"
  )
})

test_that("InferCovariateType() throws error when long data has missing covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 43, NA, NA, NA, NA)
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "Some studies do not define covariate values: Steve and Al, Frank"
  )
})

test_that("InferCovariateType() throws error when wide data has missing covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(11, NA, NA)
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "Some studies do not define covariate values: Steve and Al, Frank"
  )
})

test_that("InferCovariateType() throws error when long data has non-numerical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c("11", "11", 9, 9, "77", "77")
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "One or more covariate values are non-numerical\\."
  )
})

test_that("InferCovariateType() throws error when wide data has non-numerical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c("11", 9, "77")
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "One or more covariate values are non-numerical\\."
  )
})

test_that("InferCovariateType() throws error when long data has all identical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 11, 11, 11, 11, 11)
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "Cannot analyse covariate with no variation\\."
  )
})

test_that("InferCovariateType() throws error when wide data has all identical covariate values", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(11, 11, 11)
  )
  expect_error(
    object = InferCovariateType(df, "covar.bananas"),
    regexp = "Cannot analyse covariate with no variation\\."
  )
})

test_that("InferCovariateType() returns 'binary' when only 0 & 1 in long data", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(0, 0, 1, 1, 0, 0)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "binary")
})

test_that("InferCovariateType() returns 'binary' when only 0 & 1 in wide data", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(0, 1, 0)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "binary")
})

test_that("InferCovariateType() returns 'continuous' when only 2 values in long data which aren't 0 & 1", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 11, 1, 1, 11, 11)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "continuous")
})

test_that("InferCovariateType() returns 'continuous' when only 2 values in wide data which aren't 0 & 1", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(11, 1, 11)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "continuous")
})

test_that("InferCovariateType() returns 'continuous' when more than 2 values in long data", {
  df <- data.frame(
    Study = c("Jeff et al", "Jeff et al", "Steve and Al", "Steve and Al", "Frank", "Frank"),
    covar.bananas = c(11, 11, 1, 1, 45, 45)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "continuous")
})

test_that("InferCovariateType() returns 'continuous' when more than 2 values in wide data", {
  df <- data.frame(
    Study = c("Jeff et al", "Steve and Al", "Frank"),
    covar.bananas = c(11, 1, 45)
  )
  expect_equal(InferCovariateType(df, "covar.bananas"), "continuous")
})
