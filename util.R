# Function to create treatment labels from user input - NVB

treatment_label <- function(label) {
  label_list <- read.csv(text = label, sep = "\t")
  return(label_list)
} 

# Creates summary table plot for both app and report - NVB
summary_table_plot <- function(data, metaoutcome, treatment_list) {
  longsort <- bugsnetdt(data, metaoutcome, treatment_list)
  plot <- bugsnet_sumtb(longsort, metaoutcome)
  return(plot)
}

freq_sub <- function(data, metaoutcome, excluded, treatment_list, outcome_measure, modelranfix){
  data_wide <-  entry.df(data, metaoutcome)
  data_sub <- filter(data_wide, !Study %in% excluded)  # Get subset of data to use
  treat_list <- treatment_label(treatment_list)
  freq_wrap(data_sub, treat_list, modelranfix, outcome_measure, metaoutcome, ref_alter(data, metaoutcome, excluded, treatment_list)$ref_sub)
}
