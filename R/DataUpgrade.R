
#' Replace treatment IDs with treatment names.
#' 
#' @param old_data Data frame with old data format where treatments are specified as IDs
#' @param treatment_df Data frame with 'Number' column defining treatment IDs and 'Label' column defining treatment names
#' @return Data frame in the new format with treatment names
ReplaceTreatments <- function(old_data, treatment_df) {
  upgraded <- old_data
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
  treatments = stringr::str_trim(
    stringr::str_split(treatment_names_string, ",")[[1]]
  )
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
