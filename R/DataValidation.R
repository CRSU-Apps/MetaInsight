
#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data Data frame to validate.
#' @param outcome_type Outcome type selected for the data. Either "Binary" or "Continuous".
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
ValidateUploadedData <- function(data, outcome_type) {
  if (outcome_type == "Continuous") {
    outcome_columns <- continuous_column_names
  } else if (outcome_type == "Binary") {
    outcome_columns <- binary_column_names
  } else {
    stop(glue::glue("Outcome type {outcome_type} is not recognised. Please use 'Continuous' or 'Binary'"))
  }
  
  required_columns <- outcome_columns %>%
    dplyr::filter(required)

  missing_names <- .FindMissingColumns(data, required_columns)
  
  if (length(missing_names) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Missing columns for {outcome_type} data: {paste0(missing_names, collapse = ', ')}")
      )
    )
  }
  
  numbered_columns <- required_columns %>%
    dplyr::filter(!is.na(number_group))
  
  if (!.ValidateMatchingWideColumns(data, numbered_columns)) {
    return(
      list(
        valid = FALSE,
        message = glue::glue(
          "For wide format data, numbered columns ({paste0(numbered_columns$name, collapse = ', ')}) must all have matching sequential indices, starting from 1"
        )
      )
    )
  }

  return(list(valid = TRUE, message = "Data is valid"))
}

#' Find the names of any required columns which are missing from the data frame.
#'
#' @param data Data frame in which to find missing columns.
#' @param required_columns Data frame defining required columns. Columns in this data frame are:
#' - pattern = A regular expression defining the generic column title for wide or long format.
#' - name = The generic name of the column.
#'
#' @return Vector containing names of any columns which are required, but are missing.
.FindMissingColumns <- function(data, required_columns) {
  return(
    unlist(
      sapply(
        required_columns$pattern,
        function (pattern) {
          if (!any(grep(pattern, names(data)))) {
            return(required_columns$name[required_columns$pattern == pattern])
          } else {
            return(NULL)
          }
        }
      )
    )
  )
}

#' Validate wide columns that all columns are numbered sequentially from 1 to the same number across all numbered columns.
#'
#' @param uploaded_data Data frame to validate.
#' @param required_columns Data frame representing required numbered columns. Columns in this data frame are:
#' - pattern = A regular expression defining the generic column title for wide or long format.
#' - number_group = Regular expression replacement to extract the number from a column.
#'
#' @return TRUE if the columns are correctly numbered, else FALSE.
.ValidateMatchingWideColumns <- function(uploaded_data, required_columns) {
  # Extract all of the numbers for each column in wide format eg. T.1 -> 1, N.3 -> 3
  wide_numbers <- list()
  sapply(
    required_columns$pattern,
    function (pattern) {
      wide_numbers[[required_columns$name[required_columns$pattern == pattern]]] <<- uploaded_data %>%
        dplyr::select(grep(pattern, names(uploaded_data))) %>%
        names() %>%
        gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
        as.integer()
    }
  )
  
  # If numbers are NA for all column titles, then the data must be in long format. The rest of the checks are not relevant.
  # If only some are NA, then this data is neither long nor wide. Return FALSE for validity.
  if (all(is.na(wide_numbers))) {
    return(TRUE)
  } else if (any(is.na(wide_numbers)) != all(is.na(wide_numbers))) {
    return(FALSE)
  }
  
  # Check that all number lists are the same length
  lengths <- sapply(
    names(wide_numbers),
    function(name) {
      return(length(wide_numbers[[name]]))
    }
  )
  
  if (length(unique(lengths)) > 1) {
    return(FALSE)
  }
  
  # Checkl that all number lists are sequential from 1
  sequentials <- sapply(
    names(wide_numbers),
    function(name) {
      each_set <- sort(wide_numbers[[name]])
      return(each_set == 1:length(each_set))
    }
  )
  
  return(all(sequentials))
}
