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

# Bayesian analysis
bayesian_model <- function(sub, data, treatment_list, metaoutcome, exclusionbox, 
                           outcome_measure, modelranfix, reference_alter) {
  newData1 <- as.data.frame(data)
  treat_list <- treatment_label(treatment_list)
  longsort2 <- dataform.df(newData1, treat_list, metaoutcome) 
  if (sub == TRUE) {
    longsort2 <- filter(longsort2, !Study %in% exclusionbox)
    return(baye(longsort2, treat_list, modelranfix, outcome_measure ,metaoutcome, 
                reference_alter$ref_sub))
  } else {
    return(baye(longsort2, treat_list, modelranfix, outcome_measure ,metaoutcome, 
                reference_alter$ref_all))
  }
}
