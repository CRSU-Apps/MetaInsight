#' Creates a table of comparisons of all treatment pairs
#'
#' @param model Various model output created by baye().
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @return Relative effects table created by gemtc::relative.effect.table().
#' @export
bayes_compare <- function(model, outcome_measure){
  if (outcome_measure %in% c('OR', 'RR')) {
    return(as.data.frame(round(exp(model$rel_eff_tbl), digits = 2)))
  } else if (outcome_measure %in% c('RD', 'MD', 'SMD')) {
    return(as.data.frame(round(model$rel_eff_tbl, digits = 2)))
  } else {
    stop("outcome_measure has to be 'OR', 'RR', 'RD', 'MD', or 'SMD'.")
  }
}
