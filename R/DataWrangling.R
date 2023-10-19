
# Treatments are in priority order, such that for any study with multiple matching treatments,
# the first in this vector will be used as the reference, until the user selects another.
.potential_reference_treatments = c(
  'control',
  'usual_care',
  'standard_care',
  'placebo',
  'no_contact'
)

# Column ordering
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

.covariate_prefix <- "covar."
.covariate_prefix_regex <- "^covar\\."


#' Remove leading and trailing whitespace and collapse mutiple whictspace characters between words.
# Treatments are in priority order, such that for any study with multiple matching treatments,
# the first in this vector will be used as the reference, until the user selects another.
.potential_reference_treatments = c(
  'control',
  'usual_care',
  'standard_care',
  'placebo',
  'no_contact'
)

# Column ordering
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

.covariate_prefix <- "covar."
.covariate_prefix_regex <- "^covar\\."


#' Remove leading and trailing whitespace and collapse multiple whitespace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
}

#' Find which shape the data takes: either wide or long.
#' 
#' @param data Data for which to check shape
#' @return Either "wide" or "long"
FindDataShape <- function(data) {
  if ('T' %in% colnames(data)) {
    return("long")
  } else {
    return("wide")
  }
}

#' Convert wide format to long format (including covariate columns)
#' 
#' @param wide_data Data frame of wide format
#' @param ConBi Indicator whether outcome is binary or continuous
#' @return Data frame in long format
WideToLong <- function(wide_data, ConBi) {
  # Specify columns that contain wide data
  if (ConBi == "Continuous") {
    change_cols <- wide_data %>%
      select(starts_with(c("T","N","Mean","SD")))
  } else {
    change_cols <- wide_data %>%
      select(starts_with(c("T","N","R")))
  }
  # Transform to long
  long_data <- wide_data %>%
                  tidyr::pivot_longer(cols = names(change_cols),
                                      names_to = c(".value", "arm"),
                                      names_pattern = "(.*).(.)",
                                      values_drop_na = TRUE
  )
  return(long_data[,names(long_data)!="arm"])
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
FindAllTreatments <- function(data) {
  if (FindDataShape(data) == "long") {
    # Long format
    return(unique(data$T))
  } else {
    # Wide format
    all_treatments <- c()
    index <- 1
    col <- paste0('T.', index)
    while (col %in% colnames(data)) {
      all_treatments <- c(all_treatments, data[[col]])
      index <- index + 1
      col <- paste0('T.', index)
    }
    return(unique(all_treatments[!is.na(all_treatments)]))
  }
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
  treatment_names <- VectorWithItemFirst(all_treatments, reference_treatment)
  return(data.frame(Number = 1:length(treatment_names), Label = treatment_names))
}

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

#' Replace all of the treatment names in the data with IDs, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @param treatent_ids Data frame containing treatment names (Label) and IDs (Number)
#' @return Data frame where the treatments are given as IDs, not names
ReplaceTreatmentIds <- function(data, treatent_ids) {
  if (FindDataShape(data) == "long") {
    # Long format
    data$T <- treatent_ids$Number[match(data$T, treatent_ids$Label)]
  } else {
    # Wide format
    index <- 1
    col <- paste0('T.', index)
    while (col %in% colnames(data)) {
      data[[col]] <- treatent_ids$Number[match(data[[col]], treatent_ids$Label)]
      index <- index + 1
      col <- paste0('T.', index)
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

#' Reorder data frame columns to the correct order, both for long and wide formats.
#' 
#' @param data Data frame to reorder
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#' @return Data frame with columns reordered
ReorderColumns <- function(data, outcome_type) {
  if (tolower(outcome_type) == "continuous") {
    expected_order <- .continuous_order
  } else if (tolower(outcome_type) == "binary") {
    expected_order <- .binary_order
  } else {
    stop(paste0("Outcome type ", outcome_type, " not recognised. Use either 'Continuous' or 'Binary'"))
  }
  
  actual_order <- colnames(data)
  reordering_indices <- match(expected_order, actual_order)
  reordering_indices <- reordering_indices[!is.na(reordering_indices)]
  
  covariate_column_names <- FindCovariateNames(data)
  covariate_column_indices <- match(covariate_column_names, names(data))
  
  reordering_indices <- c(reordering_indices, covariate_column_indices)
  
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
    ReplaceTreatmentIds(treatment_ids) %>%
    AddStudyIds() %>%
    ReorderColumns(outcome_type)
  
  return(new_df)
}

.covariate_prefix <- "covar."
.covariate_prefix_regex <- "^covar\\."

FindCovariateNames <- function(df) {
  return(names(dplyr::select(df, dplyr::matches(.covariate_prefix_regex))))
}

GetFriendlyCovariateName <- function(column_name) {
  return(stringr::str_replace(column_name, .covariate_prefix_regex, ""))
}
