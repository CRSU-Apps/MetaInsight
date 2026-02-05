#' Takes bugsnet data and produces a summary dataframe of the studies
#'
#' @param bugsnet_data dataframe. Dataframe produced by `bugsnetdata()`
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
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
