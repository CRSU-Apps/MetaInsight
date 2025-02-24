#' @param freq List of NMA results created by freq_wrap().
#' @param modelranfix "fixed" or "random".
#' @param rankopts "good" or "bad", referring to smaller outcome values.
#' @return Ranking table created by netmeta::netleague().
#' @export
freq_compare <- function(freq, modelranfix, rankopts) {
  league <- netmeta::netleague(freq$net1, comb.random = (modelranfix == "random"),
                               comb.fixed = (modelranfix == "fixed"), digits = 2,
                               seq = netmeta::netrank(freq$net1, small = rankopts)
  )
  if (modelranfix == "random") {
    leaguedf <- as.data.frame(league$random)
  } else if (modelranfix == "fixed") {
    leaguedf <- as.data.frame(league$fixed)
  } else {
    stop("modelranfix must be 'fixed' or 'random'")
  }
  return(leaguedf)
}
