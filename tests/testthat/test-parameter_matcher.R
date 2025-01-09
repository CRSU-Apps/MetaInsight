
test_that("Should match empty when empty", {
  matcher <- ParameterMatcher$new()
  
  testthat::expect_true(matcher$Matches())
})

test_that("Should match identical parameters", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  
  testthat::expect_true(matcher$Matches(a=1, b=2))
})

test_that("Should match identical parameters in different order", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)

  testthat::expect_true(matcher$Matches(b=2, a=1))
})

test_that("Should match identical parameters after overwriting", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  matcher$SetParameters(a=11, b=22)

  testthat::expect_true(matcher$Matches(a=11, b=22))
})

test_that("Should match identical parameters after clearing and rewriting", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  matcher$Clear()
  matcher$SetParameters(a=11, b=22)

  testthat::expect_true(matcher$Matches(a=11, b=22))
})

test_that("Should match empty after clearing", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  matcher$Clear()
  
  testthat::expect_true(matcher$Matches())
})

test_that("Should not match anything when empty", {
  matcher <- ParameterMatcher$new()
  
  testthat::expect_false(matcher$Matches(a=1))
})

test_that("Should not match different values", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  
  testthat::expect_false(matcher$Matches(a=2, b=1))
})

test_that("Should not match same values with extra values", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  
  testthat::expect_false(matcher$Matches(a=1, b=2, c=3))
})

test_that("Should not match same values with missing values", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  
  testthat::expect_false(matcher$Matches(a=1))
})

test_that("Should not match same values with different names", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  
  testthat::expect_false(matcher$Matches(aa=1, bb=2))
})

test_that("Should not match original parameters after clearing and rewriting", {
  matcher <- ParameterMatcher$new()
  matcher$SetParameters(a=1, b=2)
  matcher$Clear()
  matcher$SetParameters(a=11, b=22)
  
  testthat::expect_false(matcher$Matches(a=1, b=2))
})
