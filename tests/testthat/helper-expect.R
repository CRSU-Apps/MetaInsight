
expect_equal_and_not_na <- function(actual, expected, item_name, equality_expectation=expect_equal) {
  # Unbox single item lists
  if (typeof(actual) == "list" && length(actual) == 1) {
    actual = unlist(actual)
  }
  if (typeof(expected) == "list" && length(expected) == 1) {
    expected = unlist(expected)
  }
  
  expect_equal(typeof(actual), typeof(expected))
  
  expect_false(all(is.na(actual), label = glue::glue("Actual {item_name} should not be NA")))
  expect_false(all(is.null(expected), label = glue::glue("Expected {item_name} should not be NA")))
  
  expect_false(all(is.na(actual), label = glue::glue("Actual {item_name} should not be NULL")))
  expect_false(all(is.null(expected), label = glue::glue("Expected {item_name} should not be NULL")))
  
  equality_expectation(actual, expected)
}

expect_data_frames_equal <- function(actual, expected, numerical_tolerance=0.0002) {
  if (class(actual) != "data.frame" || class(expected) != "data.frame") {
    stop(glue::glue("Expected data frames; found: [{paste0(class(actual), collapse = ', ')}] and [{paste0(class(expected), collapse = ', ')}]"))
  }
  
  expect_equal(nrow(actual), nrow(expected))
  expect_equal(ncol(actual), ncol(expected))
  expect_vector_equal(names(actual), names(expected))
  
  matches <- lapply(
    1:nrow(expected),
    function(rowIndex) {
      row_matches <- rep(TRUE, nrow(expected))
      # This block checks that every row in the expected data frame matches exactly one row in the actual data frame
      for (name in names(expected)) {
        if (is.na(expected[[name]][rowIndex])) {
          ## NAs match
          row_matches <- row_matches & is.na(actual[[name]])
        } else if (is.numeric(expected[[name]][rowIndex])) {
          # Numbers within tolerance
          row_matches <- row_matches & (abs(actual[[name]] - expected[[name]][rowIndex]) <= numerical_tolerance)
        } else {
          # Everything else identical
          row_matches <- row_matches & actual[[name]] == expected[[name]][rowIndex]
        }
      }
      
      return(length(which(row_matches)) == 1)
    }
  )
  
  expect_true(all(unlist(matches)))
}

expect_matrices_equal <- function(actual, expected, numerical_tolerance=0.0002) {
  if (!"matrix" %in% class(actual) || !"matrix" %in% class(expected)) {
    stop(glue::glue("Expected matrices; found: [{paste0(class(actual), collapse = ', ')}] and [{paste0(class(expected), collapse = ', ')}]"))
  }
  
  expect_equal(nrow(actual), nrow(expected))
  expect_equal(ncol(actual), ncol(expected))
  
  matches <- lapply(
    1:nrow(expected),
    function(xIndex) {
      row_matches <- rep(TRUE, nrow(expected))
      
      # This block checks that every cell in the expected matric matches the actual matrix
      for (yIndex in 1:ncol(expected)) {
        if (is.na(expected[xIndex, yIndex])) {
          row_matches <- row_matches & is.na(actual[xIndex, yIndex])
        } else if (is.numeric(expected[xIndex, yIndex])) {
          row_matches <- row_matches & (abs(actual[xIndex, yIndex] - expected[xIndex, yIndex]) <= numerical_tolerance)
        } else {
          row_matches <- row_matches & actual[xIndex, yIndex] == expected[xIndex, yIndex]
        }
      }
      
      return(all(row_matches))
    }
  )
  
  expect_true(all(unlist(matches)))
}

expect_vector_equal <- function(actual, expected, enforce_order=FALSE) {
  if (!is.vector(actual) || !is.vector(expected)) {
    stop(glue::glue("Items are not vectors"))
  }
  
  if (enforce_order) {
    expect_equal(names(actual), names(expected))
    
    expect_equal(actual, expected)
  } else {
    expect_true(all(names(actual) %in% names(expected)))
    expect_true(all(names(expected) %in% names(actual)))
    
    sapply(
      names(expected),
      function(name) {
        expect_equal(actual[[name]], expected[[name]])
      }
    )
  }
}
