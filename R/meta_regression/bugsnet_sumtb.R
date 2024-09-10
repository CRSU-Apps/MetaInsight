#' Creates a BUGSnet summary table.
#'
#' @param data Long format data.
#' @param metaoutcome "Continuous" or "Binary".
#' @return Network created by BUGSnet::net.tab().
bugsnet_sumtb <- function(data, metaoutcome){
  data.rh <- BUGSnet::data.prep(arm.data = data, varname.t = "T", varname.s = "Study")
  if (metaoutcome == "Continuous") {
    outcome = "Mean"
    typeO = "continuous"
  } else if (metaoutcome == "Binary") {
    outcome = "R"
    typeO = "binomial"
  } else {
    stop("metaoutcome must be 'Binary' or 'Continuous'")
  }
  network.char <- BUGSnet::net.tab(data = data.rh,
                                   outcome = outcome,
                                   N = "N",
                                   type.outcome = typeO,
                                   time = NULL)
  return(network.char$network)
}