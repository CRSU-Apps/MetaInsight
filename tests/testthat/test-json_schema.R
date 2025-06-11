library(jsonvalidate)

source("data/cinema_data/json.R")

test_that("Should identify valid binary json", {
  expect_true(jsonvalidate::json_validate(json_valid_binary, schema, verbose = TRUE))
})

test_that("Should identify valid continuous json", {
  expect_true(jsonvalidate::json_validate(json_valid_continuous, schema, verbose = TRUE))
})

test_that("Should identify incorrect types", {
  result <- jsonvalidate::json_validate(json_incorrect_types, schema, verbose = TRUE)
  
  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 7)
  expect_equal(attr(result, "errors"), json_incorrect_types_errors)
})

test_that("Should identify mixed outcome type", {
  result <- jsonvalidate::json_validate(json_data_mixed_outcome_type, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 6)
  expect_equal(attr(result, "errors"), json_data_mixed_outcome_type_errors)
})

test_that("Should identify both outcome type", {
  result <- jsonvalidate::json_validate(json_data_both_outcome_type, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 6)
  expect_equal(attr(result, "errors"), json_data_both_outcome_type_errors)
})

test_that("Should identify missing fields", {
  result <- jsonvalidate::json_validate(json_missing_fields, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 8)
  expect_equal(attr(result, "errors"), json_missing_fields_errors)
})

test_that("Should identify values out of bounds", {
  result <- jsonvalidate::json_validate(json_values_out_of_bounds, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 14)
  expect_equal(attr(result, "errors"), json_values_out_of_bounds_errors)
})

test_that("Should identify short arrays", {
  result <- jsonvalidate::json_validate(json_short_arrays, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 4)
  expect_equal(attr(result, "errors"), json_short_arrays_errors)
})

test_that("Should identify duplicate row and column names", {
  result <- jsonvalidate::json_validate(json_duplicate_row_column_names, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 2)
  expect_equal(attr(result, "errors"), json_duplicate_row_column_names_errors)
})

test_that("Should identify empty strings", {
  result <- jsonvalidate::json_validate(json_empty_strings, schema, verbose = TRUE)

  expect_false(result)
  expect_equal(nrow(attr(result, "errors")), 8)
  expect_equal(attr(result, "errors"), json_empty_strings_errors)
})
