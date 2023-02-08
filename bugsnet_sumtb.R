library(BUGSnet)

bugsnet_sumtb <- function(data, metaoutcome){
  data.rh<-data.prep(arm.data=data, varname.t = "T", varname.s="Study")
  if (metaoutcome=="Continuous") {
    outcome = "Mean"
    typeO= "continuous"
  } else {
    outcome = "R"
    typeO = "binomial"
  }
  network.char <- net.tab(data = data.rh,
                          outcome = outcome,
                          N = "N",
                          type.outcome = typeO,
                          time = NULL)
  return(network.char$network)
}