
#' Remove leading and trailing whitespace and collapse mutiple whictspace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
clean_data <- function(data) {
  return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
find_all_treatments <- function(data) {
  if ('T' %in% colnames(data)) {
    # Long format
    return(unique(data$T))
  } else {
    # Wide format
    all_treatments <- c()
    for (col in paste0('T.', seq(6))) {
      if (col %in% colnames(data)) {
        all_treatments <- c(all_treatments, data[[col]])
      } else {
        break
      }
    }
    return(unique(all_treatments[!is.na(all_treatments)]))
  }
}

#' Create a copy of a vector with the given item as the first element.
#' 
#' @param vector Vector to reorder
#' @param first_item The element to push to the front of the vector
#' @return The reordered vector
vector_with_item_first <- function(vector, first_item) {
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
create_treatment_ids <- function(all_treatments, reference_treatment = all_treatments[1]) {
  treatment_names = vector_with_item_first(all_treatments, reference_treatment)
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
find_expected_reference_treatment <- function(treatments) {
  expected_reference_treatments <- match(.potential_reference_treatments, tolower(treatments))
  expected_reference_treatments <- expected_reference_treatments[!is.na(expected_reference_treatments)]
  if (length(expected_reference_treatments) > 0) {
    return(treatments[expected_reference_treatments[1]])
  } else {
    return(NULL)
  }
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
replace_treatment_ids <- function(data, treatent_ids) {
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
add_study_ids <- function(data) {
  # Gather studies and assign IDs
  study_names <- unique(data$Study)
  study_df <- data.frame(id = seq(length(study_names)), name = study_names)
  
  # Add study IDs to data frame
  data$StudyID <- study_df$id[match(data$Study, study_df$name)]
  
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
reorder_columns <- function(data, outcome_type) {
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
wrangle_upload_data_to_app_data <- function(data, treatment_ids, outcome_type) {
  new_df <- data %>%
    replace_treatment_ids(treatment_ids) %>%
    add_study_ids() %>%
    reorder_columns(outcome_type)
  
  return(new_df)
}