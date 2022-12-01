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

# Creates network connectivity info displayed under network plots (1c) 
make_netconnect <- function(freq) {   
  d1 <- freq$d1
  nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
  print(nc1)
}

# Creates text displayed under forest plots (2a)
texttau <- function(freq, outcome_measure, modelranfix){      
  tau <- round(freq$net1$tau,2)
  return(tau.df(tau, freq$net1$k, freq$net1$n, modelranfix, outcome_measure))
}

make_refText = function(ref) {
  y <- paste("All outcomes are versus the reference treatment:", ref)
  return(y)
}
