#' Creates a table of comparisons of all treatment pairs
#'
#' @param model Model output created by bayes_model().
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return Relative effects table created by gemtc::relative.effect.table().
#' @export
bayes_compare <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model")){
    logger |> writeLog(type = "error", "model must be an object created by bayes_model()")
    return()
  }

  if (model$outcome_measure %in% c("OR", "RR")) {
    return(as.data.frame(round(exp(model$rel_eff_tbl), digits = 2)))
  } else if (model$outcome_measure %in% c("RD", "MD", "SMD")) {
    return(as.data.frame(round(model$rel_eff_tbl, digits = 2)))
  }
}
