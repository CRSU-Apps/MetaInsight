
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
        message = paste0(
          "Missing columns for ",
          outcome_type,
          " data: ",
          paste0(missing_names, collapse = ", ")
        )
      )
    )
  }
  
  numbered_columns <- required_columns %>%
    dplyr::filter(!is.na(number_group))
  
  if (!.ValidateMatchingWideColumns(data, numbered_columns)) {
    return(
      list(
        valid = FALSE,
        message = paste0(
          "For wide format data, numbered columns (",
          paste0(numbered_columns$name, collapse = ", "),
          ") must all have matching sequential indices, starting from 1"
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
  wide_column_nas <- unlist(
    sapply(
      required_columns$pattern,
      function (pattern) {
        return(
          uploaded_data %>%
            dplyr::select(grep(pattern, names(uploaded_data))) %>%
            names() %>%
            gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
            as.integer() %>%
            is.na() %>%
            all()
        )
      }
    )
  )
  
  if (all(wide_column_nas)) {
    return(TRUE)
  } else if (any(wide_column_nas) != all(wide_column_nas)) {
    return(FALSE)
  }
  
  wide_column_counts <- unlist(
    sapply(
      required_columns$pattern,
      function (pattern) {
        return(
          uploaded_data %>%
            dplyr::select(grep(pattern, names(uploaded_data))) %>%
            names() %>%
            length()
        )
      }
    )
  )
  
  if (length(unique(wide_column_counts)) > 1) {
    return(FALSE)
  }
  
  wide_column_maxes <- sapply(
    required_columns$pattern,
    function (pattern) {
      column_numbers <- uploaded_data %>%
        dplyr::select(grep(pattern, names(uploaded_data))) %>%
        names() %>%
        gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
        as.integer() %>%
        max()
    }
  )
  
  if (length(unique(wide_column_maxes)) > 1) {
    return(FALSE)
  }
  
  wide_column_sequentials <- sapply(
    required_columns$pattern,
    function (pattern) {
      column_numbers <- uploaded_data %>%
        dplyr::select(grep(pattern, names(uploaded_data))) %>%
        names() %>%
        gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
        as.integer() %>%
        sort()
      return(column_numbers == 1:length(column_numbers))
    }
  )
  
  return(all(wide_column_sequentials))
}
