#' @param freq list. NMA results created by freq_wrap().
#' @param outcome_measure character. "MD", "SMD", "OR", "RR", or "RD".
#' @param header numeric. Multiplier for size of text in treatment contrast headers.
#' @param title numeric. Multiplier for size of title.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#'
#' @return Plot created by groupforest.df().
#' @export
summary_study <- function(freq, outcome_measure, header, title, logger = NULL) {

  check_param_classes(c("freq", "outcome_measure", "header", "title"),
                      c("list", "character", "numeric", "numeric"), logger)

  if (!outcome_measure %in% c("MD", "SMD", "OR", "RR", "RD")){
    logger %>% writeLog(type = "error", "outcome_measure must be either MD, SMD, OR, RR or RD")
    return()
  }

  return(groupforest.df(d1 = freq$d0, ntx = freq$ntx, lstx = freq$lstx, outcome = outcome_measure,
                        HeaderSize = header, TitleSize = title))
}
