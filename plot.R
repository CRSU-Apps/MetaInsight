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

# 1c Network plot - number of trials on line
make_netgraph <- function(freq, label_size) {  
  return(netgraph(freq$net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=label_size, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen"))
}

# 1c Network plot - number of trials by nodesize and line thickness
make_netplot <- function(bugsnetdt, label_size) {
  data.rh<-data.prep(arm.data=bugsnetdt, varname.t = "T", varname.s="Study")
  return(net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex=label_size))
}

# 1c Creates network connectivity info displayed under network plots 
make_netconnect <- function(freq) {   
  d1 <- freq$d1
  nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
  print(nc1)
}

# 2a. Forest Plot
make_netComp <- function(freq, modelranfix, ref, min, max) {    
  return(forest.df(freq$net1, modelranfix, freq$lstx, ref, min, max))
}

# 2a. Creates text displayed under forest plots 
texttau <- function(freq, outcome_measure, modelranfix){      
  tau <- round(freq$net1$tau,2)
  return(tau.df(tau, freq$net1$k, freq$net1$n, modelranfix, outcome_measure))
}

make_refText = function(ref) {
  y <- paste("All outcomes are versus the reference treatment:", ref)
  return(y)
}

# 2b Treatment comparison and rank table
make_netrank <- function(freq, modelranfix, rankopts) {
  league <- netleague(freq$net1, 
                      comb.random=(modelranfix=="random"), comb.fixed = (modelranfix=="fixed"), 
                      digits =2, seq= netrank(freq$net1, small = rankopts))
  if (modelranfix=="random"){
    leaguedf<- as.data.frame(league$random)
  }
  else {
    leaguedf<- as.data.frame(league$fixed)
  }
  return(leaguedf)
}

# 2c Inconsistency

make_Incon <- function(freq, modelranfix) {
  incona <- netsplit(freq$net1)
  return(netsplitresult.df(incona, modelranfix))
}

# 3a Forest plot (Bayesian)
make_Forest <- function(model, metaoutcome, bayesmin, bayesmax) {
  if (metaoutcome=="Binary") {
    return(forest(model$mtcRelEffects, digits=3, xlim=c(log(bayesmin), log(bayesmax))))
  } else if (metaoutcome=="Continuous") {
    return(forest(model$mtcRelEffects, digits=3, xlim=c(bayesmin, bayesmax)))
  }
}







