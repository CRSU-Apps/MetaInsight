
test_that("Covariate title NA when not available", {
  df <- read.csv("Cont_long.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_true(is.na(covariate_title()))
    }
  )
})

test_that("Covariate title extracted from data when available", {
  df <- read.csv("Cont_long_cov.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_equal(covariate_title(), "covar.age")
    }
  )
})
test_that("Covariate name NA when not available", {
  df <- read.csv("Cont_long.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_true(is.na(covariate_name()))
    }
  )
})

test_that("Covariate name extracted from data when available", {
  df <- read.csv("Cont_long_cov.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_equal(covariate_name(), "age")
      expect_equal(output$subtitle, "Covariate: age")
    }
  )
})

test_that("Covariate presence status passed back to module parent when not available", {
  df <- read.csv("Cont_long.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_equal(length(session$returned), 1)
      expect_true(session$returned())
    }
  )
})

test_that("ovariate presence status passed back to module parent when available", {
  df <- read.csv("Cont_long_cov.csv") %>%
    CleanData()
  testServer(
    meta_regression_tab_server,
    args = list(all_data = function() { df }),
    {
      expect_equal(length(session$returned), 1)
      expect_false(session$returned())
    }
  )
})
