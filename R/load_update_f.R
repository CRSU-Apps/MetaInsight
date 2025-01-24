#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data_path character. path to the file to be upgraded
#' @param treatments character vector. List of the treatments in the data
#'
#' @return Dataframe containing the upgraded data
#' @export
load_update <- function(data_path, treatments, logger = NULL){

  # TO DO add input checks
  # data_path is string, ends in .csv
  # treatments is character vector

  data <- read.table(
    file = data_path,
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "\"",
    fileEncoding = "UTF-8-BOM"
  )

  treatments_df <- CreateTreatmentsDataFrame(treatments)

  input_treatment_name_count <- nrow(treatments_df)
  data_treatment_name_count <- max(FindAllTreatments(data))

  treatment_name_surplus <- input_treatment_name_count - data_treatment_name_count

  if (treatment_name_surplus != 0){
    logger %>% writeLog(type = "error", glue::glue("Your input data contains {input_treatment_name_count} treatments
                                                   but your treatment list contains {data_treatment_name_count} treatments"))
    return()
  }

  updated_data <- UpgradeData(data, treatments_df)

  updated_data
}

#' Replace treatment IDs with treatment names.
#'
#' @param old_data Data frame with old data format where treatments are specified as IDs
#' @param treatment_df Data frame with 'Number' column defining treatment IDs and 'Label' column defining treatment names
#' @return Data frame in the new format with treatment names
ReplaceTreatments <- function(old_data, treatment_df) {
  upgraded <- CleanData(old_data)
  if ('T' %in% colnames(upgraded)) {
    # Long format
    upgraded$T <- treatment_df$Label[match(upgraded$T, treatment_df$Number)]
  } else {
    # Wide format
    for (col in paste0('T.', seq(6))) {
      if (!(col %in% colnames(upgraded))) {
        break
      }
      upgraded[[col]] <- treatment_df$Label[match(upgraded[[col]], treatment_df$Number)]
    }
  }
  return(upgraded)
}

#' Create data frame containing treatment IDs and names.
#'
#' @param treatment_names_string String containing treatment names, separated by commas
#' @return Data frame with 'Number' column defining treatment IDs and 'Label' column defining treatment names
CreateTreatmentsDataFrame <- function(treatment_names_string) {
  treatments <- stringr::str_split(treatment_names_string, pattern = ",")[[1]] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(pattern = " ", replacement = "_")
  treatments <- treatments[treatments != ""]

  if (length(treatments) == 0) {
    return(data.frame())
  }

  treatment_df = data.frame(
    Number = seq(length(treatments)),
    Label = treatments
  )
  return(treatment_df)
}

#' Upgrade data frame to new format using treatment names instead of treatment IDs.
#'
#' @param old_data Data frame with old data format where treatments are specified as IDs
#' @param treatment_df Data frame with 'Number' column defining treatment IDs and 'Label' column defining treatment names
#' @return Data frame in the new format with treatment names
UpgradeData <- function(old_data, treatment_df) {
  new_data <- ReplaceTreatments(old_data, treatment_df)
  new_data$StudyID <- NULL

  return(new_data)
}
