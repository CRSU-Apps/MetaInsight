#####
# Plot functions used in both app and report - NVB
#####

source("fn_analysis.R") # Contains groupforest.df, forest.df - should probably organise better NVB

# 1a Summary table plot 
summary_table_plot <- function(bugsnetdt, metaoutcome) {
  return(bugsnet_sumtb(bugsnetdt, metaoutcome))
}

# 1b Forest plot
make_netStudy <- function(freq, outcome_measure, ForestHeader, ForestTitle) {
  return(groupforest.df(freq$d0, freq$ntx, freq$lstx, outcome_measure, ForestHeader, ForestTitle))
}

# 1c Network plot - nodesize
make_netgraph <- function(freq, label_size) {  
  return(netgraph(freq$net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=label_size, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen"))
}

# 2a. Forest Plot
make_netComp <- function(freq, modelranfix, ref, min, max) {    
  return(forest.df(freq$net1, modelranfix, freq$lstx, ref, min, max))
}