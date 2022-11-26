# Function to create treatment labels from user input - NVB

treatment_label <- function(label) {
  label_list <- read.csv(text = label, sep = "\t")
  return(label_list)
} 

# Plot functions used in both app and report - NVB

# Summary table plot 
summary_table_plot <- function(data, metaoutcome, treatment_list) {
  longsort <- bugsnetdt(data, metaoutcome, treatment_list)
  plot <- bugsnet_sumtb(longsort, metaoutcome)
  return(plot)
}

# Forest plot for all studies
make_netStudy <- function(data, metaoutcome, excluded, treatment_list, outcome_measure, modelranfix, ForestHeader, ForestTitle) {
  freq <- freq_sub(data, metaoutcome, excluded, treatment_list, outcome_measure, modelranfix)
  groupforest.df(freq$d0, freq$ntx, freq$lstx, outcome_measure, ForestHeader, ForestTitle)
}

# Network plot - nodesize
make_netgraph = function(freq,label_size) {  
  netgraph(freq$net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=label_size, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen"
  )
}



