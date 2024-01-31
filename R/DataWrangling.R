
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
# (\\.([0-9]+))? = Optional group of full stop, followed by at least one digit
# $ = End of string
# (.+) = Group of at least one character
.continuous_column_names <- data.frame() %>%
  rbind(data.frame(name = "Study", pattern = "^(?i)Study(\\.([0-9]+))?$", replacement ="Study\\1", required = TRUE, number_group = NA)) %>%
  rbind(data.frame(name = "T", pattern = "^(?i)T(\\.([0-9]+))?$", replacement ="T\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "N", pattern = "^(?i)N(\\.([0-9]+))?$", replacement ="N\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "Mean", pattern = "^(?i)Mean(\\.([0-9]+))?$", replacement ="Mean\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "SD", pattern = "^(?i)SD(\\.([0-9]+))?$", replacement ="SD\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "covar.*", pattern = "^(?i)covar\\.(.+)$", replacement ="covar.\\1", required = FALSE, number_group = NA))

.binary_column_names <- data.frame() %>%
  rbind(data.frame(name = "Study", pattern = "^(?i)Study(\\.([0-9]+))?$", replacement ="Study\\1", required = TRUE, number_group = NA)) %>%
  rbind(data.frame(name = "T", pattern = "^(?i)T(\\.([0-9]+))?$", replacement ="T\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "R", pattern = "^(?i)R(\\.([0-9]+))?$", replacement ="R\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "N", pattern = "^(?i)N(\\.([0-9]+))?$", replacement ="N\\1", required = TRUE, number_group = "\\2")) %>%
  rbind(data.frame(name = "covar.*", pattern = "^(?i)covar\\.(.+)$", replacement ="covar.\\1", required = FALSE, number_group = NA))

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

ValidateData <- function(data, outcome_type) {
  # dataShape <- FindDataShape(data)
  # if (is.null(dataShape)) {
  #   return(list(valid = FALSE, message = "Cannot identify data shape: Missing column \"T\" or \"T.1\""))
  # }
  
  if (outcome_type == "Continuous") {
    outcome_columns <- .continuous_column_names
  } else if (outcome_type == "Binary") {
    outcome_columns <- .binary_column_names
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

# .ValidateMatchingWideColumns <- function(uploaded_data, required_columns) {
#   
#   uploaded_data <<- uploaded_data
#   required_columns <<- required_columns
#   
#   # wide_column_numbers <- unlist(
#   #   sapply(
#   #     required_columns$name,
#   #     function (name) {
#   #       pattern <- required_columns$pattern[required_columns$name == name]
#   #       column_numbers <- uploaded_data %>%
#   #         dplyr::select(grep(pattern, names(uploaded_data))) %>%
#   #         names() %>%
#   #         gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
#   #         as.integer()
#   #     }
#   #   )
#   # )
#   
#   wide_column_numbers <- sapply(
#     required_columns$name,
#     function (name) {
#       pattern <- required_columns$pattern[required_columns$name == name]
#       column_numbers <- uploaded_data %>%
#         dplyr::select(grep(pattern, names(uploaded_data))) %>%
#         names() %>%
#         gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
#         as.integer()
#     }
#   )
#   
#   wide_column_numbers <<- wide_column_numbers
#   
#   if (all(is.na(wide_column_numbers))) {
#     return(TRUE)
#   }
#   
#   browser()
#   
#   all_rows_match <- all(
#     sapply(
#       1:nrow(wide_column_numbers),
#       function(row) {
#         return(
#           length(unique(wide_column_numbers[row, ])) == 1
#         )
#       }
#     )
#   )
#   
#   if (!all_rows_match) {
#     return(FALSE)
#   }
#   
#   all_columns_sequential <- all(wide_column_numbers[, 1] == 1:length(wide_column_numbers[, 1]))
#   
#   return(all_columns_sequential)
#   
#   # unique_column_numbers <- unique(wide_column_numbers)
#   # column_number_variants <- unique(
#   #   sapply(
#   #     1:ncol(unique_column_numbers),
#   #     function(col) {
#   #       return(list(unique_column_numbers[, col]))
#   #     }
#   #   )
#   # )
#   #   
#   # if (length(column_number_variants) > 1) {
#   #   return(FALSE)
#   # }
#   # 
#   # return(all(column_number_variants[1] == 1:length(column_number_variants)))
#   
#   # return(
#   #   unlist(
#   #     sapply(
#   #       required_columns$pattern,
#   #       function (pattern) {
#   #         column_numbers <- data %>%
#   #           dplyr::select(grep(pattern, names(data))) %>%
#   #           names() %>%
#   #           gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) %>%
#   #           as.integer()
#   #       }
#   #     )
#   #   )
#   # )
# }
