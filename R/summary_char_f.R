#' Takes bugsnet data and produces a summary dataframe of the studies
#'
#' @param bugsnet_data dataframe. Dataframe produced by `bugsnetdata()`
#' @param outcome character. Outcome type for the dataset. Either `Binary` or
#' `Continuous`.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return dataframe of characteristics
#' @export
summary_char <- function(bugsnet_data, outcome, logger = NULL) {

  check_param_classes(c("bugsnet_data", "outcome"), c("data.frame", "character"), logger = logger)

  if (!outcome %in% c("Binary", "Continuous")){
    logger %>% writeLog(type = "error", "outcome must be either Binary or Continuous")
    return()
  }

  data.rh <- BUGSnet::data.prep(arm.data = bugsnet_data, varname.t = "T", varname.s = "Study")
  if (outcome == "Continuous") {
    outcome = "Mean"
    typeO = "continuous"
  } else if (outcome == "Binary") {
    outcome = "R"
    typeO = "binomial"
  } else {
    logger %>% writeLog(type = "error", "outcome must be either Binary or Continuous")
    return()
  }
  network.char <- BUGSnet::net.tab(data = data.rh,
                                   outcome = outcome,
                                   N = "N",
                                   type.outcome = typeO,
                                   time = NULL)
  return(network.char$network)
}
