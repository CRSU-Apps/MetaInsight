
#' Remove leading and trailing whitespace and collapse mutiple whictspace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
FindAllTreatments <- function(data) {
  # Regular expression explanation:
  # ^ = Start of string
  # (?i) = Ignore case for matching
  # (\\.[0-9]+)? = Optional group of full stop, followed by at least one digit
  # $ = End of string
  treatment_column_matches <- stringr::str_match(names(data), "^(?i)T(\\.[0-9]+)?$")
  treatment_column_names <- treatment_column_matches[!is.na(treatment_column_matches)]
  
  treatment_names = unlist(
    sapply(
      treatment_column_names,
      function (nom) {
        treatments <- data[[nom]]
        return(treatments[!is.na(treatments)])
      }
    )
  )
  
  # Wrapped in c() to convert to an unnamed vector
  return(unique(c(treatment_names)))
}

#' Create a copy of a vector with the given item as the first element.
#' 
#' @param vector Vector to reorder
#' @param first_item The element to push to the front of the vector
#' @return The reordered vector
VectorWithItemFirst <- function(vector, first_item) {
  if (is.null(first_item) || !(first_item %in% vector)) {
    return(vector)
  }
  return(c(first_item, vector[vector != first_item]))
}

#' Create a data frame with treatment IDs and treatment names.
#' 
#' @param all_treatments Vector of all treatment names
#' @param reference_treatment Name of treatment to be assigned ID 1
#' @return Data frame containing the treatment ID ('Number') and the treatment name ('Label')
CreateTreatmentIds <- function(all_treatments, reference_treatment = all_treatments[1]) {
  treatment_names = VectorWithItemFirst(all_treatments, reference_treatment)
  return(data.frame(Number = 1:length(treatment_names), Label = treatment_names))
}

# Treatments are in priority order, such that for any study with multiple matching treatments,
# the first in this vector will be used as the reference, until the user selects another.
.potential_reference_treatments = c(
  'control',
  'usual_care',
  'standard_care',
  'placebo',
  'no_contact'
)

#' Find the expected reference treatment from a vector.
#' This is done by comparing treatment names to expected reference treatment names.
#' 
#' @param treatments vector containing all treatment names
#' @return Name of the expected reference treatment if one is found, else NULL
FindExpectedReferenceTreatment <- function(treatments) {
  expected_reference_treatments <- match(.potential_reference_treatments, tolower(treatments))
  expected_reference_treatments <- expected_reference_treatments[!is.na(expected_reference_treatments)]
  if (length(expected_reference_treatments) > 0) {
    return(treatments[expected_reference_treatments[1]])
  } else {
    return(NULL)
  }
}

# Regular expression explanation:
# ^ = Start of string
# (?i) = Ignore case for matching
# (\\.[0-9]+)? = Optional group of full stop, followed by at least one digit
# $ = End of string
# (.+) = Group of at least one character
.continuous_column_names <- c(
  "^(?i)Study(\\.[0-9]+)?$" = "Study\\1",
  "^(?i)T(\\.[0-9]+)?$" = "T\\1",
  "^(?i)N(\\.[0-9]+)?$" = "N\\1",
  "^(?i)Mean(\\.[0-9]+)?$" = "Mean\\1",
  "^(?i)SD(\\.[0-9]+)?$" = "SD\\1",
  "^(?i)covar\\.(.+)$" = "covar.\\1"
)
.binary_column_names <- c(
  "^(?i)Study(\\.[0-9]+)?$" = "Study\\1",
  "^(?i)T(\\.[0-9]+)?$" = "T\\1",
  "^(?i)R(\\.[0-9]+)?$" = "R\\1",
  "^(?i)N(\\.[0-9]+)?$" = "N\\1",
  "^(?i)covar\\.(.+)$" = "covar.\\1"
)

#' Rename the columns of a data frame to match the expected letter casing.
#'
#' @param data Data frame to fix
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#'
#' @return Data frame with renamed columns.
.FixColumnNameCases <- function(data, outcome_type) {
  if (outcome_type == "Continuous") {
    column_names <- .continuous_column_names
  } else if (outcome_type == "Binary") {
    column_names <- .binary_column_names
  } else {
    stop(glue::glue("Outcome type {outcome_type} is not recognised. Please use 'Continuous' or 'Binary'"))
  }
  
  corrected_names <- unlist(
    sapply(
      names(data),
      function (name) {
        return(.CorrectColumnName(name, column_names))
      }
    )
  )
  
  names(data) <- corrected_names
  
  return(data)
}

#' Correct a column name to match the expected letter casing.
#'
#' @param original_name Column name to fix
#' @param column_names Named vector where each name is a regular expression to match, and the value is the replacement string.
#'
#' @return The corrected column name.
.CorrectColumnName <- function(original_name, column_names) {
  matches <- unlist(
    sapply(
      names(column_names),
      function(nom) {
        if (length(grep(nom, original_name)) > 0) {
          column_names[[nom]]
        } else {
          NULL
        }
      }
    )
  )
  
  if (length(matches) > 0) {
    return(
      sub(
        names(matches)[1],
        matches[1],
        original_name
      )
    )
  }
  
  return(original_name)
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
ReplaceTreatmentIds <- function(data, treatent_ids) {
  if ('T' %in% colnames(data)) {
    # Long format
    data$T <- treatent_ids$Number[match(data$T, treatent_ids$Label)]
  } else {
    # Wide format
    for (col in paste0('T.', seq(6))) {
      if (col %in% colnames(data)) {
        data[[col]] <- treatent_ids$Number[match(data[[col]], treatent_ids$Label)]
      } else {
        break
      }
    }
  }
  return(data)
}

#' Add a new column in the data for study IDs, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
AddStudyIds <- function(data) {
  study_names <- unique(data$Study)
  
  # Add study IDs to data frame
  data$StudyID <- match(data$Study, study_names)
  
  return(data)
}

.common_order = c("StudyID", "Study")
.continuous_specific_order = unlist(
  lapply(
    c("", paste0(".", 1:6)),
    function(x) paste0(c("T", "N", "Mean", "SD"), x)
  )
)
.binary_specific_order = unlist(
  lapply(
    c("", paste0(".", 1:6)),
    function(x) paste0(c("T", "R", "N"), x)
  )
)

.continuous_order <- c(.common_order, .continuous_specific_order)
.binary_order <- c(.common_order, .binary_specific_order)

#' Reorder data frame columns to the correct order, both for long and wide formats.
#' 
#' @param data Data frame to reorder
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#' @return Data frame with columns reordered
ReorderColumns <- function(data, outcome_type) {
  if (outcome_type == "Continuous") {
    expected_order <- .continuous_order
  } else if (outcome_type == "Binary") {
    expected_order <- .binary_order
  } else {
    stop(paste0("Outcome type ", outcome_type, " not recognised. Use either 'Continuous' or 'Binary'"))
  }
  
  actual_order <- colnames(data)
  reordering_indices <- match(expected_order, actual_order)
  reordering_indices <- reordering_indices[!is.na(reordering_indices)]
  
  return(data[, reordering_indices])
}

#' Wrangle the uploaded data into a form usable by the internals of the app, both for long and wide formats.
#' 
#' @param data Data frame to wrangle
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#' @return Data frame which is uasable by the rest of the app
WrangleUploadData <- function(data, treatment_ids, outcome_type) {
  new_df <- data %>%
    .FixColumnNameCases(outcome_type) %>%
    ReplaceTreatmentIds(treatment_ids) %>%
    AddStudyIds() %>%
    ReorderColumns(outcome_type)
  
  return(new_df)
}
