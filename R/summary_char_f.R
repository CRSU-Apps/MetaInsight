#' Produce a summary dataframe of the studies using `BUGSnet::net.tab()`
#'
#' @inheritParams common_params
#' @return dataframe of characteristics
#' @export
summary_char <- function(configured_data, logger = NULL) {

  check_param_classes(c("configured_data"), c("configured_data"), logger)

  data.rh <- BUGSnet::data.prep(arm.data = configured_data$bugsnet, varname.t = "T", varname.s = "Study")
  if (configured_data$outcome == "continuous") {
    bugsnet_outcome <- "Mean"
    typeO <- "continuous"
  } else {
    bugsnet_outcome <- "R"
    typeO <- "binomial"
  }

  network.char <- BUGSnet::net.tab(data = data.rh,
                                   outcome = bugsnet_outcome,
                                   N = "N",
                                   type.outcome = typeO,
                                   time = NULL)
  return(network.char$network)
}
