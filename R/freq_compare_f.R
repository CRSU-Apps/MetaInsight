#' Produce a comparison table of treatments using
#' `netmeta::netleague()`.
#'
#' @inheritParams common_params
#' @return Dataframe of comparisons with one row and one column
#' per treatment
#' @export
freq_compare <- function(configured_data, logger = NULL) {

  check_param_classes(c("configured_data"),
                      c("configured_data"), logger)

  league <- netmeta::netleague(configured_data$freq$net1, random = (configured_data$effects == "random"),
                               common = (configured_data$effects == "fixed"), digits = 2,
                               seq = netmeta::netrank(configured_data$freq$net1,
                                                      small = configured_data$ranking_option))

  if (configured_data$effects == "random") {
    league_df <- as.data.frame(league$random)
  }
  if (configured_data$effects == "fixed") {
    league_df <- as.data.frame(league$fixed)
  }

  return(league_df)
}
