
.valid_result <- list(valid = TRUE, message = "Data is valid")

#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data Data frame to validate.
#' @param outcome_type Outcome type selected for the data. Either "Binary" or "Continuous".
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
ValidateUploadedData <- function(data, outcome_type) {
  if (is.null(data) || nrow(data) == 0) {
    return(
      list(
        valid = FALSE,
        message = "File is empty"
      )
    )
  }
  
  if (outcome_type == "Continuous") {
    outcome_columns <- continuous_column_names
  } else if (outcome_type == "Binary") {
    outcome_columns <- binary_column_names
  } else {
    stop(glue::glue("Outcome type {outcome_type} is not recognised. Please use 'Continuous' or 'Binary'"))
  }
  
  required_columns <- outcome_columns %>%
    dplyr::filter(required)

  result <- .ValidateMissingColumns(data, required_columns, outcome_type)
  if (!result$valid) {
    return(result)
  }

  result <- .ValidateNumberedColumns(data, required_columns)
  if (!result$valid) {
    return(result)
  }

  result <- .ValidateColumnTypes(data, outcome_columns)
  if (!result$valid) {
    return(result)
  }

  result <- .ValidateSingleArmStudies(data)
  if (!result$valid) {
    return(result)
  }

  result <- .ValidateQualityColumns(data)
  if (!result$valid) {
    return(result)
  }
  
  return(.valid_result)
}

#' Validate that there are no missing columns in the data.
#'
#' @param data Data frame to validate.
#' @param required_columns Data frame containing data definitions.
#' @param outcome_type Outcome type selected for the data. Either "Binary" or "Continuous".
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateMissingColumns <- function(data, required_columns, outcome_type) {
  missing_names <- .FindMissingColumns(data, required_columns)
  
  if (length(missing_names) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Missing columns for {outcome_type} data: {paste0(missing_names, collapse = ', ')}")
      )
    )
  }
  
  return(.valid_result)
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

#' Validate that numbered columns in the data are seqential from 1.
#'
#' @param data Data frame to validate.
#' @param required_columns Data frame containing data definitions.
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateNumberedColumns <- function(data, required_columns) {
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
  
  return(.valid_result)
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
  
  # Check that all number lists are sequential from 1
  sequentials <- sapply(
    names(wide_numbers),
    function(name) {
      each_set <- sort(wide_numbers[[name]])
      return(each_set == 1:length(each_set))
    }
  )
  
  return(all(sequentials))
}

#' Validate that all columns in the data are of the expected type.
#'
#' @param data Data frame to validate.
#' @param outcome_columns Data frame containing data definitions.
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateColumnTypes <- function(data, outcome_columns) {
  mistyped_columns <- .FindMistypedColumns(data, outcome_columns)
  
  if (length(mistyped_columns) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Some columns have incorrect data types: {paste0(mistyped_columns, collapse = ', ')}")
      )
    )
  }
  
  return(.valid_result)
}

#' Identify any columns which are not of the expected type.
#'
#' @param data Data frame to validate.
#' @param outcome_columns Data frame containing data definitions.
#'
#' @return A vector of column titles which contain data of the wrong type.
.FindMistypedColumns <- function(data, outcome_columns) {
  mistyped_columns = c()
  sapply(
    1:nrow(outcome_columns),
    function (index) {
      column_definition <- outcome_columns[index, ]
      matching_column_names = grep(column_definition$pattern, names(data), value = TRUE)
      
      new_mistyped_columns <- unlist(
        sapply(
          matching_column_names,
          function(column_name) {
            is_typed_correctly <- do.call(what = column_definition$type_check, args = list(data[[column_name]]))
            if (!is_typed_correctly) {
              return(glue::glue("{column_name} should be of type {column_definition$type_name}"))
            }
          }
        )
      )
      mistyped_columns <<- c(mistyped_columns, new_mistyped_columns)
    }
  )
  return(mistyped_columns)
}

#' Validate that all studies have at least 2 arms.
#'
#' @param data Data frame to validate.
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateSingleArmStudies <- function(data) {
  all_studies <- unique(data$Study)
  single_arm_studies <- unlist(
    lapply(
      all_studies,
      function(study) {
        if (length(FindAllTreatments(data = data, study = study)) < 2) {
          return(study)
        }
        return()
      }
    )
  )

  if (length(single_arm_studies) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Some studies have single arms: {paste0(single_arm_studies, collapse = ', ')}")
      )
    )
  }

  return(.valid_result)
}

#' Validate that the quality assessment columns in the data are of the expected format.
#'
#' @param data Data frame to validate.
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateQualityColumns <- function(data) {
  if (is.null(data$rob) && is.null(data$indirectness)) {
    return(.valid_result)
  }
  
  if (xor(is.null(data$rob), is.null(data$indirectness))) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Provide both 'rob' and 'indirectness', or neither.")
      )
    )
  }

  studies_with_wrong_qa_values <- unique(data$Study[!(data$rob %in% 1:3) | !(data$indirectness %in% 1:3)])
  if (!is.null(data$rob) && !is.null(data$indirectness)
      && length(studies_with_wrong_qa_values) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Some studies have values for 'rob' or 'indirectness' that are not 1, 2 or 3: {paste0(studies_with_wrong_qa_values, collapse = ', ')}")
      )
    )
  }
  
  #Find the studies that have more than one value for risk of bias, by creating a table of rob values by study and summing the rows to check if any rows have more than one non-zero element.
  rob_table <- table(data[, c("Study", "rob")])
  studies_with_multiple_rob <- dimnames(rob_table)$Study[as.vector(rowSums(rob_table != 0) > 1)]
  if (length((studies_with_multiple_rob) > 0)) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Some studies do not have the same risk of bias value for every arm: {paste0(studies_with_multiple_rob, collapse = ', ')}.")
      )
    )
  }
  
  #Find the studies that have more than one value for indirectness, by creating a table of indirectness values by study and summing the rows to check if any rows have more than one non-zero element.
  indirectness_table <- table(data[, c("Study", "indirectness")])
  studies_with_multiple_indirectness <- dimnames(indirectness_table)$Study[as.vector(rowSums(indirectness_table != 0) > 1)]
  if (length(studies_with_multiple_indirectness) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Some studies do not have the same indirectness value for every arm: {paste0(studies_with_multiple_indirectness, collapse = ', ')}.")
      )
    )
  }
  
  return(.valid_result)
}