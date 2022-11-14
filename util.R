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


