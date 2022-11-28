#####
# Contains utility functions
# May want to re-organise location later
# File created by NVB
#####

# Function to create treatment labels from user input - NVB
treatment_label <- function(label) {
  label_list <- read.csv(text = label, sep = "\t")
  return(label_list)
} 


# Reference treatment if treatment 1 is removed from the network
ref_alter <- function(data, metaoutcome, excluded, treatment_list){
  newData1 <- as.data.frame(data)
  treat_list <- treatment_label(treatment_list)
  lstx <- treat_list$Label
  ref_all <- as.character(lstx[1])
  longsort2 <- dataform.df(newData1, treat_list, metaoutcome)
  long_sort2_sub <- filter(longsort2, !Study %in% excluded)  # subgroup
  if (((lstx[1] %in% long_sort2_sub$T) ) == "TRUE") {
    ref_sub<- as.character(lstx[1])
  } else {
    ref_sub <- as.character(long_sort2_sub$T[1])
  }
  reference <- list(ref_all=ref_all, ref_sub=ref_sub)
  return(reference)
}



