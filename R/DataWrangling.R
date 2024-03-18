
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

#' Convert wide format to long format (including covariate columns)
#' 
#' @param wide_data Data frame of wide format
#' @param outcome_type Indicator whether outcome is 'Binary' or 'Continuous'
#' @return Data frame in long format
WideToLong <- function(wide_data, outcome_type) {
  # Specify columns that contain wide data
  if (outcome_type == "Continuous") {
    change_cols <- wide_data %>%
      dplyr::select(tidyselect::starts_with(c("T", "N", "Mean", "SD")))
  } else if (outcome_type == "Binary") {
    change_cols <- wide_data %>%
      dplyr::select(tidyselect::starts_with(c("T", "R", "N")))
  } else {
    paste0("outcome_type needs to be 'Binary' or 'Continuous'")
  }
  # Transform to long
  long_data <- wide_data %>%
                  tidyr::pivot_longer(cols = names(change_cols),
                                      names_to = c(".value"),
                                      names_pattern = "^(.*)\\.[0-9]+$",
                                      values_drop_na = TRUE
  )
  long_data <- long_data %>% dplyr::relocate(FindCovariateNames(long_data), .after = last_col())
  return(as.data.frame(long_data))
}

#' Convert long format to wide format (including covariate columns)
#' 
#' @param long_data Data frame of long format
#' @param outcome_type Indicator whether outcome is 'Binary' or 'Continuous'
#' @return Data frame in wide format
LongToWide <- function(long_data, outcome_type) {
  # Specify columns that contain wide data
  if (outcome_type == "Continuous") {
    change_cols <- long_data %>%
      dplyr::select(c("T", "N", "Mean", "SD"))
  } else if (outcome_type == "Binary") {
    change_cols <- long_data %>%
      dplyr::select(c("T", "R", "N"))
  } else {
    paste0("outcome_type needs to be 'Binary' or 'Continuous'")
  }
  # Add arms
  long_data <- long_data %>% dplyr::group_by(Study) %>% dplyr::mutate(arm = dplyr::row_number())
  # Transform to long
  wide_data <- long_data %>%
    tidyr::pivot_wider(id_cols = c("Study", FindCovariateNames(long_data)),
                       names_from = c("arm"),
                       values_from = names(change_cols),
                       names_sep = "."
    )
  return(as.data.frame(wide_data))
}

#' Create a copy of a data from which does not contain any covariate columns.
#' 
#' @param data Data from which to remove covariate columns
#' @return Data without covariate columns
RemoveCovariates <- function(data) {
  covariate_column_names <- FindCovariateNames(data)
  
  if (length(covariate_column_names) == 0) {
    return(data)
  }
  
  covariate_column_indices <- match(covariate_column_names, names(data))
  return(data[, -covariate_column_indices])
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

#' Find all of the treatment names in the data, both for long and wide formats.
#' If a study is specified, then the treatments are only found for that study.
#' Otherwise, treatments are found for all studies.
#' 
#' @param data Data frame in which to search for treatment names.
#' @param treatment_ids Data frame containing names of treatments ("Label") and IDs ("Number").
#' This is only needed if the "T" column in the data frame contains IDs instead of names. Defaults to NULL.
#' @param study Name of study for which to find treatment names. Defaults to NULL.
#' @return Vector of all treatment names.
FindAllTreatments <- function(data, treatment_ids = NULL, study = NULL) {
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
        if (is.null(study)) {
          treatments <- data[[nom]]
        } else {
          treatments <- data[[nom]][data$Study == study]
        }
        return(treatments[!is.na(treatments)])
      }
    )
  )
  
  # Wrapped in c() to convert to an unnamed vector
  if (is.null(treatment_ids)) {
    return(unique(c(treatment_names)))
  } else {
    return(unique(c(treatment_ids$Label[match(treatment_names, treatment_ids$Number)])))
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

#' Rename the columns of a data frame to match the expected letter casing.
#'
#' @param data Data frame to fix
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#'
#' @return Data frame with renamed columns.
.FixColumnNameCases <- function(data, outcome_type) {
  if (outcome_type == "Continuous") {
    column_names <- continuous_column_names
  } else if (outcome_type == "Binary") {
    column_names <- binary_column_names
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
      column_names$pattern,
      function(pattern) {
        if (length(grep(pattern, original_name)) > 0) {
          column_names$replacement[column_names$pattern == pattern]
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
#' @param treatent_ids Data frame containing treatment names (Label) and IDs (Number)
#' @return Data frame where the treatments are given as IDs, not names
ReplaceTreatmentIds <- function(data, treatment_ids) {
  if (FindDataShape(data) == "long") {
    # Long format
    data$T <- treatment_ids$Number[match(data$T, treatment_ids$Label)]
  } else {
    # Wide format
    index <- 1
    col <- paste0('T.', index)
    while (col %in% colnames(data)) {
      data[[col]] <- treatment_ids$Number[match(data[[col]], treatment_ids$Label)]
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
    .FixColumnNameCases(outcome_type) %>%
    ReplaceTreatmentIds(treatment_ids) %>%
    AddStudyIds() %>%
    ReorderColumns(outcome_type)
  
  return(new_df)
}

#' Clean the treatment labels to replace all characters which are not a number, letter, or underscore, with an underscore.
#' 
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @return Cleaned version of treatment_ids
CleanTreatmentIds <- function(treatment_ids) {
  new_treatment_ids <- treatment_ids
  new_treatment_ids$RawLabel <- treatment_ids$Label
  new_treatment_ids$Label <- treatment_ids$Label %>%
    stringr::str_replace_all("(?![a-zA-Z0-9_]).", "_") %>%
    stringr::str_replace_all("(_+)", "_")
  
  return(new_treatment_ids)
}

#' Find the names of all columns which contain a covariate.
#'
#' @param df Data frame in which to find covariate columns.
#' @return Names of all covariate columns
FindCovariateNames <- function(df) {
  return(names(dplyr::select(df, dplyr::matches(.covariate_prefix_regex))))
}

#' Convert a covariate column name to a display name.
#'
#' @param column_name Covariate column name to convert
#' @return Friendly covariate name
GetFriendlyCovariateName <- function(column_name) {
  return(stringr::str_replace(column_name, .covariate_prefix_regex, ""))
}



#' Keep or delete rows in @param data corresponding to the control treatment in each study.
#' 
#' @param data Data in long format, plus the column 'Treatment', a text version of 'T'.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param keep_delete "keep" or "delete".
#' @return @param data with rows corresponding to the control treatment kept or deleted, and a new column 'Control'.
KeepOrDeleteControlTreatment <- function(data, treatments, keep_delete){
  #It is imperative that 'data' is sorted by 'Study', because tapply below will sort by 'Study', creating a misalignment between 'Study' and 'Control' in the 'control' data frame if 'data' is not sorted by 'Study' already.
  data <- dplyr::arrange(data, Study, T)
  #The unique studies
  studies <- unique(data$Study)
  #Local function to find the control treatment in a single study
  #When used in tapply it matches the treatments within a study to the ordered 'treatments' vector, and then finds the lowest
  min_match <- function(x){
    min(match(x, treatments))
  }
  #Find the control treatment in each study
  control <- data.frame(Study = studies, Control = treatments[tapply(data$Treatment, INDEX = data$Study, FUN = min_match)])
  data <- merge(data, control, by = "Study", sort = FALSE)
  if (keep_delete == "keep"){
    return(data[data$Treatment == data$Control, ])
  } else if (keep_delete == "delete"){
    return(data[data$Treatment != data$Control, ])
  } else{
    stop("keep_delete must be 'keep' or 'delete'")
  }
}



#' Get the outcome in the reference arm when it exists
#' 
#' @param data Data in long format, plus the column 'Treatment', a text version of 'T'.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Binary" or "Continuous".
#' @return Vector of reference arm outcomes, named by study.
GetReferenceOutcome <- function(data, treatments, outcome_type){
  #Data with only control treatment rows kept
  data_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "keep")
  if (outcome_type == "Binary"){
    data_control$R[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "PLO",
                                    xi = data_control$R,
                                    ni = data_control$N)
  } else if (outcome_type == "Continuous"){
    data_control$Mean[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "MN",
                                    mi = data_control$Mean,
                                    sdi = data_control$SD,
                                    ni = data_control$N)
  } else{
    stop("'outcome_type' must be 'Continuous' or 'Binary'")
  }
  outcomes <- as.numeric(effect_sizes$yi)
  names(outcomes) <- unique(data_control$Study)
  return(outcomes)
}

