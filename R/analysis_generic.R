# Inputting the data in long form
# (Essentially a wrapper for dataform.df, which converts data into long format, adds the variable 'se' in the continuous case, and does some formatting.)
bugsnetdata <- function(data, metaoutcome, treatment_list){
  newData1 <- as.data.frame(data)
  treatment_list$Label <- stringr::str_wrap(gsub("_", " ",treatment_list$Label), width=10)  # better formatting (although does assume underscores have only been added due to the treatment label entry limitations) CRN
  longsort2 <- dataform.df(newData1,treatment_list,metaoutcome)    
  return(longsort2)
}



# Reference treatment if treatment 1 is removed from the network
ref_alter <- function(data, metaoutcome, excluded, treatment_list){
  newData1 <- as.data.frame(data)
  lstx <- treatment_list$Label
  ref_all <- as.character(lstx[1])
  longsort2 <- dataform.df(newData1, treatment_list, metaoutcome)
  long_sort2_sub <- filter(longsort2, !Study %in% excluded)  # subgroup
  if (((lstx[1] %in% long_sort2_sub$T) ) == "TRUE") {
    ref_sub<- as.character(lstx[1])
  } else {
    ref_sub <- as.character(long_sort2_sub$T[1])
  }
  return(list(ref_all=ref_all, ref_sub=ref_sub))
}



####################################
# Function for choosing default ordering in example datasets #
#####################################

RankingOrder <- function(outcome,data) {
  file1 <- data
  if (outcome=="Binary" & is.null(file1)==TRUE) {
    choice <- "bad"
  }
  else {
    choice <- "good"
  }
  return(choice)
}



### data transform
dataform.df <- function(newData1, treat_list, CONBI) {
  if (FindDataShape(newData1) == "long") {
    long <- newData1
  } else {
    data_wide <- newData1
    a <- ifelse(CONBI == 'Continuous', 4, 3)
    numbertreat <- (ncol(newData1) - 2) / a
    
    # Add more columns to make data up to 6 columns
    if (numbertreat < 6) {
      for (k in (numbertreat + 1):6) {
        if (CONBI == 'Continuous') {
          data_wide[c(paste0("T.", k), paste0("N.", k), paste0("Mean.", k), paste0("SD.", k))] <- NA
        } else {
          data_wide[c(paste0("T.", k), paste0("R.", k), paste0("N.", k))] <- NA
        }
      }
    } else {
      data_wide <- newData1
    }
    long_pre <- reshape(
      data_wide,
      direction = "long",
      varying = 3:ncol(data_wide), 
      times = c(".1", ".2", ".3", ".4", ".5", ".6"),
      sep = ".",
      idvar =  c("StudyID", "Study")
    )
    long_pre <- subset(long_pre, select = -time)
    long <- long_pre[!is.na(long_pre$T), ]
  }
  long_sort <- long[order(long$StudyID, -long$T), ]
  if (CONBI == 'Continuous') {
    long_sort$se<-long_sort$SD / sqrt(long_sort$N)
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


