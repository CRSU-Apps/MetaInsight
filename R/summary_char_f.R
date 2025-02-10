#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data dataframe. Uploaded data
#' @param treatment_df vector of treatments
#' @param reference_treatment character. The reference treatment
#' @return list
#' @export
summary_char <- function(data, outcome) {

  data.rh <- BUGSnet::data.prep(arm.data = data, varname.t = "T", varname.s = "Study")
  if (outcome == "Continuous") {
    outcome = "Mean"
    typeO = "continuous"
  } else if (outcome == "Binary") {
    outcome = "R"
    typeO = "binomial"
  } else {
    stop("outcome must be 'Binary' or 'Continuous'")
  }
  network.char <- BUGSnet::net.tab(data = data.rh,
                                   outcome = outcome,
                                   N = "N",
                                   type.outcome = typeO,
                                   time = NULL)
  return(network.char$network)
}
