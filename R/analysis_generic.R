#' Reformats the treatment labels then calls dataform.df().
#'
#' @param data Input dataset.
#' @param metaoutcome "Continuous" or "Binary".
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @return Output from dataform.df().
bugsnetdata <- function(data, metaoutcome, treatment_list){
  newData1 <- as.data.frame(data)
  treatment_list$Label <- stringr::str_wrap(gsub("_", " ", treatment_list$Label), width = 10)  # better formatting (although does assume underscores have only been added due to the treatment label entry limitations) CRN
  longsort2 <- dataform.df(newData1 = newData1,
                           treat_list = treatment_list,
                           CONBI = metaoutcome)
  return(longsort2)
}



####################################
# Function for choosing default ordering in example datasets #
#####################################

#' Returns the default ranking order, for the example datasets.
#'
#' @param outcome "Binary" or "Continuous".
#' @param data Input dataset.
#' @return "good" or "bad".
RankingOrder <- function(outcome, data) {
  file1 <- data
  if (outcome == "Binary" & is.null(file1)) {
    choice <- "bad"
  } else {
    choice <- "good"
  }
  return(choice)
}


#' Converts wide data to long, adds the variable 'se' for a continuous outcomes, and does some formatting.
#' (TM: to be modified to use WideToLong() when it becomes available from meta-regression.)
#'
#' @param newData1 Input dataset.
#' @param treat_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param CONBI "Continuous" or "Binary".
#' @return Input data set in long format with the variable 'se' for a continuous outcome.
dataform.df <- function(newData1, treat_list, CONBI) {
  if (FindDataShape(newData1) == "long") {
    long <- newData1
  } else {
    long <- WideToLong(wide_data = newData1, outcome = CONBI)
  }

  long_sort <- long[order(long$StudyID, -long$T), ]
  if (CONBI == 'Continuous') {
    long_sort$se <- long_sort$SD / sqrt(long_sort$N)
  }

  long_sort <- ReinstateTreatmentIds(data = long_sort, treatment_ids = treat_list)

  return(long_sort)
}

#' Find the titles of all varying column to be reshaped.
#'
#' @param wide_data Data frame win which to find varying column names.
#'
#' @return Vector of titles of all numbered columns.
.FindVaryingColumnIndices <- function(wide_data) {
  return(grep("^(?:T|R|N|SD|Mean)\\.([0-9]+)$", names(wide_data)))
}

#' Find the number of treatment columns in a wide data frame. This assumes that all columns are numbered sequentially, starting at 1.
#'
#' @param wide_data Data frame in which to find treatment column count.
#'
#' @return The number of treatment columns.
.FindTreatmentCount <- function(wide_data) {
  matches <- stringr::str_match(names(wide_data), "^T\\.([0-9]+)$")[, 2]
  matches <- matches[!is.na(matches)]

  return(max(as.integer(matches)))
}
