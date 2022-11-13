
# bugsnetdt <- function(data, metaoutcome, treatment_labels){
#   newData1 <- as.data.frame(data)
#   label <- treatment_labels
#   treat_list <- read.csv(text=label, sep = "\t")
#   longsort2 <- dataform.df(newData1,treat_list,metaoutcome)    # inputting the data in long form
#   return(longsort2)
# }

# Inputting the data in long form
source("fn_analysis.R")

bugsnetdt <- function(data, metaoutcome, treatment_list){
  newData1 <- as.data.frame(data)
  treat_list <- treatment_label(treatment_list)
  longsort2 <- dataform.df(newData1,treat_list,metaoutcome)    
  return(longsort2)
}