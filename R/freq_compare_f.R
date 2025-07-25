#' Produce comparison tables
#'
#' @param freq List of NMA results created by freq_wrap().
#' @param modelranfix Character. `fixed` or `random`.
#' @param rankopts Character. `good` or `bad`, referring to smaller outcome values.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return Ranking table created by netmeta::netleague().
#' @export
freq_compare <- function(freq, model_type, ranking_option, logger = NULL) {

  check_param_classes(c("freq", "model_type", "ranking_option"),
                      c("list", "character", "character"), logger)

  if (!model_type %in% c("fixed", "random")){
    logger |> writeLog(type = "error", "model_type must be 'fixed' or 'random'")
    return()
  }

  if (!ranking_option %in% c("good", "bad")){
    logger |> writeLog(type = "error", "ranking_option must be 'good' or 'bad'")
    return()
  }

  league <- netmeta::netleague(freq$net1, random = (model_type == "random"),
                               common = (model_type == "fixed"), digits = 2,
                               seq = netmeta::netrank(freq$net1, small = ranking_option))

  if (model_type == "random") {
    league_df <- as.data.frame(league$random)
  }
  if (model_type == "fixed") {
    league_df <- as.data.frame(league$fixed)
  }

  return(league_df)
}
