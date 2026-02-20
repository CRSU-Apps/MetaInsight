#' Assess the data for validity. This checks the column names for required columns
#' and balanced wide format numbered columns.
#'
#' @param data_path character. Path to the file to be loaded or if `NULL` load
#' the default data
#' @inheritParams common_params
#' @return List containing:
#'  \item{is_data_valid}{logical. Whether the data is valid}
#'  \item{is_data_uploaded}{logical. Whether the data is uploaded}
#'  \item{data}{dataframe. The data that was uploaded or the default data if
#'  no data_path was provided}
#'  \item{treatments}{Dataframe of the treatments in the data. `NULL` if
#'  `is_data_valid` is `FALSE`}
#'  \item{outcome}{character. Whether the data is `binary` or `continuous`}
#' @export
setup_load <- function(data_path = NULL, outcome, logger = NULL){

  # check inputs
  if (!is.null(data_path)){
    if (!inherits(data_path, "character")){
      logger |> writeLog(type = "error", "data_path must be of class character")
      return()
    }
    if (!tools::file_ext(data_path) %in% c("csv", "xlsx")){
      logger |> writeLog(type = "error", "data_path must link to either a .csv or .xlsx file")
      return()
    }
    if (!file.exists(data_path)){
      logger |> writeLog(type = "error", "The specified file does not exist")
      return()
    }
  }

  if (!inherits(outcome, "character")){
    logger |> writeLog(type = "error", "outcome must be of class character")
    return()
  }

  if (!outcome %in% c("binary", "continuous")){
    logger |> writeLog(type = "error", "outcome must be either binary or continuous")
    return()
  }

  # Use the default data if no path is provided
  if (!is.null(data_path)){
    is_uploaded <- TRUE
  } else {
    is_uploaded <- FALSE
    if (outcome == "continuous"){
      data_path <- system.file("extdata", "continuous_long.csv", package = "metainsight")
    } else {
      data_path <- system.file("extdata", "binary_long.csv", package = "metainsight")
    }
  }

  data <- tryCatch(
    {
      rio::import(data_path)
    },
    error = function(err) {
      return(NULL)
    }
  )

  is_valid <- ValidateUploadedData(data, outcome)

  # If the uploaded data is invalid, return that
  if (!is_valid$valid){
    logger |> writeLog(type = "error",
                              glue::glue("Uploaded data was invalid because: {is_valid$message}.
                                Please check you data file and ensure that you have the correct outcome type selected."))
    output <- list(is_data_valid = is_valid$valid,
                   is_data_uploaded = is_uploaded,
                   data = data,
                   treatments = NULL,
                   outcome = outcome)

    class(output) <- "loaded_data"
    return(output)

  } else {

    # Process the data further if valid
    data <- CleanData(data)
    treatment_list <- FindAllTreatments(data)
    treatments <- CreateTreatmentIds(treatment_list)

    output <- list(is_data_valid = is_valid$valid,
                   is_data_uploaded = is_uploaded,
                   data = data,
                   treatments = treatments,
                   outcome = outcome)

    class(output) <- "loaded_data"
    return(output)
  }

}

.valid_result <- list(valid = TRUE, message = "Data is valid")

#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data Data frame to validate.
#' @param outcome Outcome type selected for the data. Either "binary" or "continuous".
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
ValidateUploadedData <- function(data, outcome) {
  if (is.null(data) || nrow(data) == 0) {
    return(
      list(
        valid = FALSE,
        message = "File is empty"
      )
    )
  }

  if (outcome == "continuous") {
    outcome_columns <- continuous_column_names
  } else if (outcome == "binary") {
    outcome_columns <- binary_column_names
  } else {
    stop(glue::glue("Outcome {outcome} is not recognised. Please use 'continuous' or 'binary'"))
  }

  required_columns <- outcome_columns |>
    dplyr::filter(.data$required)

  result <- .ValidateMissingColumns(data, required_columns, outcome)
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

  covariate_name <- FindCovariateNames(data)
  if (length(covariate_name) > 0){
    result <- .ValidateCovariate(data, covariate_name)
    if (!result$valid) {
      return(result)
    }
  }

  return(.valid_result)
}

#' Validate that there are no missing columns in the data.
#'
#' @param data Data frame to validate.
#' @param required_columns Data frame containing data definitions.
#' @param outcome Outcome type selected for the data. Either "binary" or "continuous".
#'
#' @return Validation result in the form of a list:
#' - "valid" = TRUE or FALSE defining whether data is valid
#' - "message" = String describing any issues causing the data to be invalid
.ValidateMissingColumns <- function(data, required_columns, outcome) {
  missing_names <- .FindMissingColumns(data, required_columns)

  if (length(missing_names) > 0) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("Missing columns for {outcome} data: {paste0(missing_names, collapse = ', ')}")
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
  numbered_columns <- required_columns |>
    dplyr::filter(!is.na(.data$number_group))

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
      wide_numbers[[required_columns$name[required_columns$pattern == pattern]]] <<- uploaded_data |>
        dplyr::select(grep(pattern, names(uploaded_data))) |>
        names() |>
        gsub(pattern = pattern, replacement = required_columns$number_group[required_columns$pattern == pattern]) |>
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
  mistyped_columns <- c()
  sapply(
    1:nrow(outcome_columns),
    function (index) {
      column_definition <- outcome_columns[index, ]
      matching_column_names <- grep(column_definition$pattern, names(data), value = TRUE)

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
  #The rob and indirectness columns
  rob_indirectness_columns <- grep(pattern = "^rob|^indirectness$", x = names(data), value = TRUE)

  if (length(rob_indirectness_columns) == 0) {
    return(.valid_result)
  }

  #The number of individual RoB columns
  n_rob_individual <- length(grep(pattern = "^rob\\..+$", x = names(data)))

  #Check there are no more than 10 individual RoB columns
  if (n_rob_individual > 10) {
    return(
      list(
        valid = FALSE,
        message = glue::glue("A maximum of 10 individual risk of bias variables are allowed.")
      )
    )
  }

  #Loop through the rob and indirectness columns
  for (var in rob_indirectness_columns) {
    #Check for studies that have more than one value
    rob_table <- table(data[, c("Study", var)])
    studies_with_multiple_rob <- dimnames(rob_table)$Study[as.vector(rowSums(rob_table != 0) > 1)]
    if (length((studies_with_multiple_rob) > 0)) {
      return(
        list(
          valid = FALSE,
          message = glue::glue("Some studies do not have the same {var} value for every arm: {paste0(studies_with_multiple_rob, collapse = ', ')}.")
        )
      )
    }
    #Check for studies that have invalid values
    studies_with_wrong_qa_values <- unique(data$Study[!data[, var] %in% 1:3])
    if (length(studies_with_wrong_qa_values) > 0) {
      return(
        list(
          valid = FALSE,
          message = glue::glue("Some studies have values for {var} that are not 1, 2 or 3: {paste0(studies_with_wrong_qa_values, collapse = ', ')}")
        )
      )
    }
  }

  return(.valid_result)
}


# Regular expression explanation:
# ^ = Start of string
# (?i) = Ignore case for matching
# (\\.([0-9]+))? = Optional group of full stop, followed by at least one digit
# $ = End of string
# (.+) = Group of at least one character

.study_definition <- data.frame(
  name = "Study",
  required = TRUE,
  type_check = "is.character",
  type_name = "character (text)",
  pattern = "^(?i)Study(\\.([0-9]+))?$",
  replacement = "Study\\1",
  number_group = NA
)

.t_definition <- data.frame(
  name = "T",
  required = TRUE,
  type_check = "is.character",
  type_name = "character (text)",
  pattern = "^(?i)T(\\.([0-9]+))?$",
  replacement = "T\\1",
  number_group = "\\2"
)

.n_definition <- data.frame(
  name = "N",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)N(\\.([0-9]+))?$",
  replacement = "N\\1",
  number_group = "\\2"
)

.mean_definition <- data.frame(
  name = "Mean",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)Mean(\\.([0-9]+))?$",
  replacement = "Mean\\1",
  number_group = "\\2"
)

.sd_definition <- data.frame(
  name = "SD",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)SD(\\.([0-9]+))?$",
  replacement = "SD\\1",
  number_group = "\\2"
)

.covariate_definition <- data.frame(
  name = "covar.*",
  required = FALSE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)covar\\.(.+)$",
  replacement = "covar.\\1",
  number_group = NA
)

.r_definition <- data.frame(
  name = "R",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)R(\\.([0-9]+))?$",
  replacement = "R\\1",
  number_group = "\\2"
)

.rob_definition <- data.frame(
  name = "rob",
  required = FALSE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)rob(\\..+)?$",
  replacement = "rob\\1",
  number_group = NA
)

.indirectness_definition <- data.frame(
  name = "indirectness",
  required = FALSE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)indirectness$",
  replacement = "indirectness",
  number_group = NA
)

continuous_column_names <- data.frame() |>
  rbind(.study_definition) |>
  rbind(.t_definition) |>
  rbind(.n_definition) |>
  rbind(.mean_definition) |>
  rbind(.sd_definition) |>
  rbind(.covariate_definition) |>
  rbind(.rob_definition) |>
  rbind(.indirectness_definition)

binary_column_names <- data.frame() |>
  rbind(.study_definition) |>
  rbind(.t_definition) |>
  rbind(.r_definition) |>
  rbind(.n_definition) |>
  rbind(.covariate_definition) |>
  rbind(.rob_definition) |>
  rbind(.indirectness_definition)



# Treatments are in priority order, such that for any study with multiple matching treatments,
# the first in this vector will be used as the reference, until the user selects another.
.potential_reference_treatments <- c(
  'control',
  'usual_care',
  'standard_care',
  'placebo',
  'no_contact'
)

# Column ordering
.common_order <- c("StudyID", "Study")
.rob_order <- c("rob", "indirectness")

#' The column order for a continuous outcome, for wide and long data.
#'
#' @param max_arms Maximum number of arms in the data set.
#' @return Vector of column names in order.
.continuousOrder <- function(max_arms) {
  continuous_specific_order <- unlist(
    lapply(
      c("", paste0(".", 1:max_arms)),
      function(x) paste0(c("T", "N", "Mean", "SD"), x)
    )
  )
  return(c(.common_order, continuous_specific_order, .rob_order))
}

#' The column order for a binary outcome, for wide and long data.
#'
#' @param max_arms Maximum number of arms in the data set.
#' @return Vector of column names in order.
.binaryOrder <-function(max_arms) {
  binary_specific_order <- unlist(
    lapply(
      c("", paste0(".", 1:max_arms)),
      function(x) paste0(c("T", "R", "N"), x)
    )
  )
  return(c(.common_order, binary_specific_order, .rob_order))
}

.covariate_prefix <- "covar."
.covariate_prefix_regex <- "^covar\\."
.rob_individual_prefix_regex <- "^rob\\."


#' Remove leading and trailing whitespace and collapse multiple whitespace characters between words.
#'
#' @param data Data frame to clean
#' @return Cleaned data frame
CleanData <- function(data) {
  return(dplyr::mutate(data, dplyr::across(dplyr::where(is.character), .TidyStringItem)))
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
#' @param outcome Indicator whether outcome is 'binary' or 'continuous'
#' @return Data frame in long format
#' @export
WideToLong <- function(wide_data, outcome) {
  # Specify columns that contain wide data
  if (outcome == "continuous") {
    change_cols <- wide_data |>
      dplyr::select(dplyr::starts_with(c("T", "N", "Mean", "SD")))
  } else if (outcome == "binary") {
    change_cols <- wide_data |>
      dplyr::select(dplyr::starts_with(c("T", "R", "N")))
  } else {
    paste0("outcome needs to be 'binary' or 'continuous'")
  }
  # Transform to long
  long_data <- wide_data |>
    tidyr::pivot_longer(cols = names(change_cols),
                        names_to = c(".value"),
                        names_pattern = "^(.*)\\.[0-9]+$",
                        values_drop_na = TRUE
    )
  long_data <- long_data |> dplyr::relocate(FindCovariateNames(long_data), .after = dplyr::last_col())
  return(as.data.frame(long_data))
}

#' Convert long format to wide format (including covariate columns)
#'
#' @param long_data Data frame of long format
#' @param outcome Indicator whether outcome is 'binary' or 'continuous'
#' @return Data frame in wide format
LongToWide <- function(long_data, outcome) {
  # Specify columns that contain wide data
  if (outcome == "continuous") {
    change_cols <- long_data |>
      dplyr::select(c("T", "N", "Mean", "SD"))
  } else if (outcome == "binary") {
    change_cols <- long_data |>
      dplyr::select(c("T", "R", "N"))
  } else {
    paste0("outcome needs to be 'binary' or 'continuous'")
  }
  # Add arms
  long_data <- long_data |> dplyr::group_by(.data$Study) |> dplyr::mutate(arm = dplyr::row_number())
  # Transform to long
  wide_data <- long_data |>
    tidyr::pivot_wider(id_cols = c("StudyID", "Study", FindCovariateNames(long_data)),
                       names_from = c("arm"),
                       values_from = names(change_cols),
                       names_sep = "."
    )
  return(as.data.frame(wide_data))
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
#' @export
FindAllTreatments <- function(data, treatment_ids = NULL, study = NULL) {
  # Regular expression explanation:
  # ^ = Start of string
  # (?i) = Ignore case for matching
  # (\\.[0-9]+)? = Optional group of full stop, followed by at least one digit
  # $ = End of string
  treatment_column_matches <- stringr::str_match(names(data), "^(?i)T(\\.[0-9]+)?$")
  treatment_column_names <- treatment_column_matches[!is.na(treatment_column_matches)]

  treatment_names <- unlist(
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

#' Find all of the studies which include the given treatments, both for long and wide formats.
#'
#' @param data Data frame in which to search for study names.
#' @param treatments Vector of matching treatments.
#' @param all_or_any Set to "all" to return studies containing all treatments. Set to "any" to return studies containing any of the treatments.
#' @return Vector of all matching study names.
FindStudiesIncludingTreatments <- function(data, treatments, all_or_any) {
  if ("T" %in% colnames(data)) {
    # Long format
    table <- table(data[, c("Study", "T")])
    table <- table[, colnames(table) %in% treatments]
    n_treatments_in_study <- rowSums(table)
    if (all_or_any == "any") {
      return(names(n_treatments_in_study[n_treatments_in_study > 0]))
    } else if (all_or_any == "all") {
      return(names(n_treatments_in_study[n_treatments_in_study == length(treatments)]))
    }
  } else {
    # Wide format
    T_columns <- grep(pattern = "^T\\.", x = names(data), value = TRUE)
    data <- data[, c("Study", T_columns)]
    long_data <- reshape2::melt(data, id.vars = "Study")
    long_data <- long_data[, c("Study", "value")]
    names(long_data)[names(long_data) == "value"] <- "T"
    return(
      FindStudiesIncludingTreatments(
        data = long_data,
        treatments = treatments,
        all_or_any = all_or_any
      )
    )
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

#' Rename the columns of a data frame to match the expected letter casing.
#'
#' @param data Data frame to fix
#' @param outcome Type of outcome for which to reorder, either 'continuous' or 'binary'
#'
#' @return Data frame with renamed columns.
.FixColumnNameCases <- function(data, outcome) {
  if (outcome == "continuous") {
    column_names <- continuous_column_names
  } else if (outcome == "binary") {
    column_names <- binary_column_names
  } else {
    stop(glue::glue("Outcome type {outcome} is not recognised. Please use 'continuous' or 'binary'"))
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
#' @param treatment_ids Data frame containing treatment names (Label) and IDs (Number)
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
#' @export
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
#' @param outcome_type Type of outcome for which to reorder, either 'continuous' or 'binary'
#' @return Data frame with columns reordered
ReorderColumns <- function(data, outcome_type) {
  #The maximum number of arms
  max_arms <- FindMaxArms(data)
  if (tolower(outcome_type) == "continuous") {
    expected_order <- .continuousOrder(max_arms = max_arms)
  } else if (tolower(outcome_type) == "binary") {
    expected_order <- .binaryOrder(max_arms = max_arms)
  } else {
    stop(paste0("Outcome type ", outcome_type, " not recognised. Use either 'continuous' or 'binary'"))
  }

  actual_order <- colnames(data)
  reordering_indices <- match(expected_order, actual_order)
  reordering_indices <- reordering_indices[!is.na(reordering_indices)]

  covariate_column_names <- FindCovariateNames(data)
  covariate_column_indices <- match(covariate_column_names, names(data))

  rob_individual_column_names <- FindRobIndividualNames(data)
  rob_individual_column_indices <- match(rob_individual_column_names, names(data))

  reordering_indices <- c(reordering_indices, rob_individual_column_indices, covariate_column_indices)

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
#' @param outcome Type of outcome for which to reorder, either 'continuous' or 'binary'
#' @return Data frame ordered by StudyID, then T if applicable
SortByStudyIDThenT <- function(data, outcome) {
  if (FindDataShape(data) == "long") {
    return(SortLong(data))
  } else {
    return(data |>
             WideToLong(outcome) |>
             SortLong() |>
             LongToWide(outcome)
    )
  }
}

#' Wrangle the uploaded data into a form usable by the internals of the app, both for long and wide formats.
#'
#' @param data Data frame to wrangle
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome Type of outcome for which to reorder, either 'continuous' or 'binary'
#' @return Data frame which is uasable by the rest of the app
WrangleUploadData <- function(data, treatment_ids, outcome) {
  new_df <- data |>
    .FixColumnNameCases(outcome) |>
    ReplaceTreatmentIds(treatment_ids) |>
    AddStudyIds() |>
    CleanStudies() |>
    ReorderColumns(outcome) |>
    SortByStudyIDThenT(outcome)

  return(new_df)
}


#' Clean strings by replacing all characters that are not a number, letter, or underscore, with an underscore.
#'
#' @param strings Vector of the strings to be cleaned.
#' @return Cleaned version of the strings.
CleanStrings <- function(strings) {
  return(strings |>
           stringr::str_replace_all("(?![a-zA-Z0-9_]).", "_") |>
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
#' @export
FindCovariateNames <- function(df) {
  return(names(dplyr::select(df, dplyr::matches(.covariate_prefix_regex))))
}

#' Convert a covariate column name to a display name.
#'
#' @param column_name Covariate column name to convert
#' @return Friendly covariate name
#' @export
GetFriendlyCovariateName <- function(column_name) {
  return(stringr::str_replace(column_name, .covariate_prefix_regex, ""))
}

#' Find the names of all columns which contain an individual RoB variable.
#' @param df Data frame in which to find covariate columns.
#' @return Names of all individual RoB columns
FindRobIndividualNames <- function(df) {
  return(names(dplyr::select(df, dplyr::matches(.rob_individual_prefix_regex))))
}

#' Throw an error if any study meets given criteria.
#'
#' @param values covariate values in which to check.
#' @param condition Function taking the covariate values to check.
#' This should return TRUE in the error case.
#' @param message Message to show before listing the problem studies or FALSE
#' if no problems are found
.ThrowErrorForMatchingStudies <- function(values, condition, message) {
  study_conditions <- sapply(
    names(values),
    function(name) {
      condition(values[[name]])
    }
  )

  matching_studies <- names(study_conditions)[study_conditions]

  if (length(matching_studies) > 0) {
    studies_list <- glue::glue_collapse(matching_studies, sep = ", ")
    return(
      list(
        valid = FALSE,
        message = paste(message, studies_list, sep = " ")
      )
    )
  } else {
    FALSE
  }
}

#' Validate the covariate and infer the type of the covariate from the data in the column.
#'  In error cases, this function will throw exceptions:
#' - If the data has any NAs
#' - If the data has any non-numeric values
#' - If every study has the same covariate value
#' - If any study contains multiple different covariate values
#'
#' @param data Data frame containing all study data.
#' @param covariate_title Name of the covariate column.
#'
#' @return list containing `valid` and `message` to pass to logger
.ValidateCovariate <- function(data, covariate_title) {
  covariate_data <- data[[covariate_title]]

  if (!is.numeric(covariate_data)) {
    return(
      list(
        valid = FALSE,
        message = "One or more covariate values are non-numerical."
      )
    )
  }

  covariate_values <- list()
  for (study in unique(data$Study)) {
    covariate_values[[study]] <- unique(covariate_data[data$Study == study])
  }

  na_values <- .ThrowErrorForMatchingStudies(
                  values = covariate_values,
                  condition = function(study_values) {
                    any(is.na(study_values))
                  },
                  message = "Some studies do not define covariate values for all arms:"
                )

  if (inherits(na_values, "list")){
    return(na_values)
  }

  study_values <- .ThrowErrorForMatchingStudies(
                    values = covariate_values,
                    condition = function(study_values) {
                      length(study_values) > 1
                    },
                    message = "Some studies contain inconsistent covariate values between arms:"
                  )

  if (inherits(study_values, "list")){
    return(study_values)
  }

  unique_items <- unique(covariate_data)
  if (length(unique_items) == 1) {
    return(
      list(
        valid = FALSE,
        message = "Cannot analyse covariate with no variation."
      )
    )
  }

  return(.valid_result)
}

#' Find the names of all columns which contain a RoB or indirectness variable.
#' @param df Data frame in which to find covariate columns.
#' @return Names of all RoB or indirectness columns
FindRobNames <- function(df) {
  all_rob <- c(FindRobIndividualNames(df), "rob", "indirectness")
  return(c(grep("^rob\\.", names(df), value = TRUE),
    dplyr::intersect(c("rob", "indirectness"), names(df))))
}

#' Shorten RoB and indirectness variable names.
#' @param long_names Vector of risk of bias and indirectness variable names.
#' @return Vector of the same length with shortened names.
ShortenRobNames <- function(long_names) {
  short_names <- long_names
  short_names[long_names == "indirectness"] <- "ind"
  rob_individual_index <- grep(pattern = "^rob\\.", x = long_names)
  if (length(rob_individual_index) > 0) {
    short_names[rob_individual_index] <- paste0("rob", 1:length(rob_individual_index))
  }
  return(short_names)
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



