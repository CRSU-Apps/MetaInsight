
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

#' The column order for a continuous outcome, for wide and long data.
#' 
#' @param max_arms Maximum number of arms in the data set.
#' @return Vector of column names in order.
.ContinuousOrder <- function(max_arms) {
  continuous_specific_order <- unlist(
    lapply(
      c("", paste0(".", 1:max_arms)),
      function(x) paste0(c("T", "N", "Mean", "SD"), x)
    )
  )
  return(c(.common_order, continuous_specific_order))
}

#' The column order for a binary outcome, for wide and long data.
#' 
#' @param max_arms Maximum number of arms in the data set.
#' @return Vector of column names in order.
.BinaryOrder <-function(max_arms) {
  binary_specific_order <- unlist(
    lapply(
      c("", paste0(".", 1:max_arms)),
      function(x) paste0(c("T", "R", "N"), x)
    )
  )
  return(c(.common_order, binary_specific_order))
}

.covariate_prefix <- "covar."
.covariate_prefix_regex <- "^covar\\."


#' Remove leading and trailing whitespace and collapse multiple whitespace characters between words.
#' 
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  # return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
  return(dplyr::mutate(data, across(where(is.character), .TidyStringItem)))
}

#' Tidy a character column.
#' 
#' @param string A character vector to be tidied.
#' @return Tidied character vector.
.TidyStringItem <- function(string) {
  tidied <- stringr::str_squish(string)
  tidied[tidied == ""] <- NA
  return(tidied)
}

#' Find all of the treatment names in the data, both for long and wide formats.
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
    tidyr::pivot_wider(id_cols = c("StudyID", "Study", FindCovariateNames(long_data)),
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
  # Regular expression explanation:
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

#' Replace all of the treatment IDs in the data with names, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment IDs.
#' @param treatment_ids Data frame containing treatment names (Label) and IDs (Number).
#' @return Data frame where the treatments are given as names, not IDs.
ReinstateTreatmentIds <- function(data, treatment_ids) {
  if ("T" %in% colnames(data)) {
    # Long format
    data$T <- treatment_ids$Label[match(data$T, treatment_ids$Number)]
  } else {
    # Wide format
    index <- 1
    col <- paste0("T.", index)
    while (col %in% colnames(data)) {
      data[[col]] <- treatment_ids$Label[match(data[[col]], treatment_ids$Number)]
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


#' Reorder data frame columns to the correct order, both for long and wide formats.
#' 
#' @param data Data frame to reorder
#' @param outcome_type Type of outcome for which to reorder, either 'Continuous' or 'Binary'
#' @return Data frame with columns reordered
ReorderColumns <- function(data, outcome_type) {
  #The maximum number of arms
  max_arms <- FindMaxArms(data)
  if (tolower(outcome_type) == "continuous") {
    expected_order <- .ContinuousOrder(max_arms = max_arms)
  } else if (tolower(outcome_type) == "binary") {
    expected_order <- .BinaryOrder(max_arms = max_arms)
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

#' Sort long data by StudyID then T
#' 
#' @param long_data Data in long format.
#' @return Long data sorted by StudyID then T.
SortLong <- function(long_data) {
  return(long_data[order(long_data$StudyID, long_data$T), ])
}

#' Sort the data by StudyID then T
#' 
#' @param data Data frame to sort
#' @return Data frame ordered by StudyID, then T if applicable
SortByStudyIDThenT <- function(data, outcome_type) {
  if (FindDataShape(data) == "long") {
    return(SortLong(data))
  } else {
    return(data |>
             WideToLong(outcome_type) |>
             SortLong() |>
             LongToWide(outcome_type)
           )
  }
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
    CleanStudies() %>%
    ReorderColumns(outcome_type) %>%
    SortByStudyIDThenT(outcome_type)
  
  return(new_df)
}


#' Clean strings by replacing all characters that are not a number, letter, or underscore, with an underscore.
#' 
#' @param strings Vector of the strings to be cleaned.
#' @return Cleaned version of the strings.
CleanStrings <- function(strings) {
  return(strings %>%
           stringr::str_replace_all("(?![a-zA-Z0-9_]).", "_") %>%
           stringr::str_replace_all("(_+)", "_")
         )
}


#' Clean study names by replacing all characters that are not a number, letter, or underscore, with an underscore.
#' @param data Data frame with the column 'Study' that contains study names.
#' @return The data frame with 'Study' cleaned and a new column 'RawStudy' which is a copy of the original 'Study'.
CleanStudies <- function(data) {
  data$RawStudy <- data$Study
  data$Study <- CleanStrings(data$Study)
  return(data)
}


#' Clean the treatment labels to replace all characters which are not a number, letter, or underscore, with an underscore.
#' 
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @return Cleaned version of treatment_ids
CleanTreatmentIds <- function(treatment_ids) {
  new_treatment_ids <- treatment_ids
  new_treatment_ids$RawLabel <- treatment_ids$Label
  new_treatment_ids$Label <- CleanStrings(treatment_ids$Label)
  
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
  #Add a Control column to the data set
  for (i in 1:length(data$Study)) {
    data$Control[i] <- treatments[min(match(data$Treatment[data$Study == data$Study[i]], treatments))]
  }
  if (keep_delete == "keep"){
    return(data[data$Treatment == data$Control, ])
  } else if (keep_delete == "delete"){
    return(data[data$Treatment != data$Control, ])
  } else{
    stop("keep_delete must be 'keep' or 'delete'")
  }
}



#' Create a list of all columns starting with the given column prefix.
#' 
#' @param wide_data Data in wide format.
#' @param column_prefix String containing the opening letters of one or more columns in 'wide_data'.
#' @return List with one element per selected column in 'wide_data', with each element named by the column name.
CreateListOfWideColumns <- function(wide_data, column_prefix) {
  return(
    as.list(
      wide_data[, grep(pattern = paste0(column_prefix, ".*"),
                       x = names(wide_data),
                       value = TRUE)]
    )
  )
}



#' Find the maximum number of arms in a study, for long or wide data.
#' 
#' @param data Long or wide format data.
#' @return The maximum number of arms.
FindMaxArms <- function(data) {
  if (FindDataShape(data) == "long") {
    return(max(table(data$Study)))
  } else {
    return(
      length(
        grep(pattern = "T.*",
             x = names(data))
      )
    )
  }
}



#' Returns a number rounded in a nice manner.
#' 
#' @param number Any number.
#' @return 'number' with 1 decimal place or 1 significant figure.
RoundForDisplay <- function(number) {
  rounded <- round(number, digits = 1)
  if (rounded == 0) {
    return(signif(number, digits = 1))
  } else {
    return(rounded)
  }
}