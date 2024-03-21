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



#' Returns the reference treatment for the main analysis and the sensitivity analysis, which will be the same unless the main one is removed from the network in the sensitivity analysis.
#' 
#' @param data Input dataset.
#' @param metaoutcome "Continuous" or "Binary".
#' @param excluded Vector of excluded studies.
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @return List of reference treatments.
#'  - 'ref_all' = Main reference treatment.
#'  - 'ref_sub' = Sensitivity analysis reference treatment.
ref_alter <- function(data, metaoutcome, excluded, treatment_list){
  newData1 <- as.data.frame(data)
  lstx <- treatment_list$Label
  ref_all <- as.character(lstx[1])
  longsort2 <- dataform.df(newData1 = newData1,
                           treat_list = treatment_list,
                           CONBI = metaoutcome)
  long_sort2_sub <- filter(longsort2, !Study %in% excluded)  # subgroup
  if (lstx[1] %in% long_sort2_sub$T) {
    ref_sub <- ref_all
  } else {
    ref_sub <- as.character(long_sort2_sub$T[1])
  }
  return(list(ref_all = ref_all, ref_sub = ref_sub))
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
  #Decide if the data is in long or wide format
  if (ncol(newData1) == 6 | ncol(newData1) == 5) {
    long <- newData1
  } else {
    data_wide <- newData1
    #The number of columns per treatment, for wide data
    a <- ifelse(CONBI == 'Continuous', 4, 3)
    #Find the number of treatments
    numbertreat <- (ncol(newData1) - 2) / a
    #Add columns as if there were six treatments, but set values to NA for the dedundant columns
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        if (CONBI == 'Continuous') {
          data_wide[c(paste0("T.", k), paste0("N.", k), paste0("Mean.", k), paste0("SD.", k))] <- NA
        } else {
          data_wide[c(paste0("T.", k), paste0("R.", k), paste0("N.", k))] <- NA
        }
      }
    } else {
      data_wide <- newData1
    }
    #Reshape wide into long
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times = c(".1", ".2", ".3", ".4", ".5", ".6"), sep = ".", idvar = c("StudyID", "Study"))
    long_pre <- subset(long_pre, select = -time)
    long <- long_pre[!is.na(long_pre$T), ]
  }
  long_sort <- long[order(long$StudyID, -long$T), ]
  if (CONBI == 'Continuous') {
    long_sort$se <- long_sort$SD / sqrt(long_sort$N)
  }
  lstx <- treat_list$Label
  treat_list2 <- data.frame(treat_list)
  ntx <- nrow(treat_list)
  colnames(treat_list2)[1] <- "T"
  long_sort2 <- merge(long_sort, treat_list2, by = c("T"))
  long_sort2 <- subset(long_sort2, select = -T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  return(long_sort2)
} 


