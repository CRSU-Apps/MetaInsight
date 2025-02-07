#' @param freq List of NMA results created by freq_wrap().
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @param ForestHeader Multiplier for size of text in treatment contrast headers.
#' @param ForestTitle Multiplier for size of title.
#' @return Plot created by groupforest.df().
#' @export
summary_study <- function(freq, outcome_measure, ForestHeader, ForestTitle) {
  return(groupforest.df(d1 = freq$d0, ntx = freq$ntx, lstx = freq$lstx, outcome = outcome_measure,
                        HeaderSize = ForestHeader, TitleSize = ForestTitle))
}
