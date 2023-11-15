
#' Remove leading and trailing whitespace and collapse mutiple whictspace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
}

FindAllColumnNames <- function(long_form_name, wide_form_name = long_form_name, data) {
  if (long_form_name %in% colnames(data)) {
    # Long format
    return(long_form_name)
  } else {
    # Wide format
    treatment_names <- c()
    index <- 1
    col <- paste0(wide_form_name, index)
    while (col %in% colnames(data)) {
      treatment_names <- c(treatment_names, col)
      index <- index + 1
      col <- paste0(wide_form_name, index)
    }
    return(unique(treatment_names[!is.na(treatment_names)]))
  }
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
FindAllTreatments <- function(data, treatment_columns = NA) {
  if (is.na(treatment_columns)) {
    treatment_columns <- FindAllColumnNames("T", "T.", data)
  }
  all_treatment_cells <- unname(unlist(data[treatment_columns]))
  return(unique(all_treatment_cells[!is.na(all_treatment_cells)]))
}

#' Find all of the study names which include the given treatment names, both for long and wide formats.
#' 
#' @param data Data frame in which to search for study names
#' @param treatments Vector of matching treatments
#' @return Vector of all matching study names
FindStudiesIncludingTreatments <- function(data, treatments) {
  if ("T" %in% colnames(data)) {
    # Long format
    return(unique(data$Study[data$T %in% treatments]))
  } else {
    # Wide format
    studies <- c()
    index <- 1
    col <- paste0("T.", index)
    while (col %in% colnames(data)) {
      studies <- c(studies, data$Study[data[[col]] %in% treatments])
      index <- index + 1
      col <- paste0("T.", index)
    }
    return(unique(studies))
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
  treatment_names = VectorWithItemFirst(all_treatments, reference_treatment)
  return(data.frame(Number = 1:length(treatment_names), Label = treatment_names))
}

# Treatments are in priority order, such that for any study with multiple matching treatments,
# the first in this vector will be used as the reference, until the user selects another.
.potential_reference_treatments = c(
  "control",
  "usual_care",
  "standard_care",
  "placebo",
  "no_contact"
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

#' Replace all of the treatment names in the data with IDs, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @param treatent_ids Data frame containing treatment names (Label) and IDs (Number)
#' @return Data frame where the treatments are given as IDs, not names
ReplaceTreatmentIds <- function(data, treatent_ids) {
  if ("T" %in% colnames(data)) {
    # Long format
    data$T <- treatent_ids$Number[match(data$T, treatent_ids$Label)]
  } else {
    # Wide format
    index <- 1
    col <- paste0("T.", index)
    while (col %in% colnames(data)) {
      data[[col]] <- treatent_ids$Number[match(data[[col]], treatent_ids$Label)]
      index <- index + 1
      col <- paste0("T.", index)
    }
  }
  return(data)
}

#' Replace all of the treatment IDs in the data with names, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment IDs.
#' @param treatent_ids Data frame containing treatment names (Label) and IDs (Number).
#' @return Data frame where the treatments are given as names, not IDs.
ReinstateTreatmentIds <- function(data, treatent_ids) {
  if ("T" %in% colnames(data)) {
    # Long format
    data$T <- treatent_ids$Label[match(data$T, treatent_ids$Number)]
  } else {
    # Wide format
    index <- 1
    col <- paste0("T.", index)
    while (col %in% colnames(data)) {
      data[[col]] <- treatent_ids$Label[match(data[[col]], treatent_ids$Number)]
      index <- index + 1
      col <- paste0("T.", index)
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
    ReplaceTreatmentIds(treatment_ids) %>%
    AddStudyIds() %>%
    ReorderColumns(outcome_type)
  
  return(new_df)
}