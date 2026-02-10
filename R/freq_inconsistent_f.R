#' Produce inconsistency tables using `netmeta::netsplit()`
#'
#' @inheritParams common_params
#'
#' @return Dataframe of inconsistency data:
#' \item{Comparison}{Treatment comparison}
#' \item{No.Studies}{Number of studies}
#' \item{NMA}{NMA treatment effect estimate}
#' \item{Direct}{Direct treatment effect estimate}
#' \item{Indirect}{Indirect treatment effect estimate}
#' \item{Difference}{Difference between treatment effects}
#' \item{Diff_95CI_lower}{2.5% limit of difference in treatment effects}
#' \item{Diff_95CI_upper}{97.5% limit of difference in treatment effects}
#' \item{pValue}{p-value for test of "difference in treatment effects == 0"}
#' @export
freq_inconsistent <- function(configured_data, logger = NULL) {

  check_param_classes(c("configured_data"),
                      c("configured_data"), logger)

  incona <- netmeta::netsplit(configured_data$freq$net1)

  Comparison <- incona$comparison
  No.Studies <- as.integer(incona$k)

  if (configured_data$effects == "random") {
    Direct <- incona$direct.random$TE
    Indirect <- incona$indirect.random$TE
    Difference <- incona$compare.random$TE
    Diff_95CI_lower <- incona$compare.random$lower
    Diff_95CI_upper <- incona$compare.random$upper
    NMA <- incona$random$TE
    pValue <- incona$compare.random$p
  }
  if (configured_data$effects == "fixed") {
    Direct <- incona$direct.fixed$TE
    Indirect <- incona$indirect.fixed$TE
    Difference <- incona$compare.fixed$TE
    Diff_95CI_lower <- incona$compare.fixed$lower
    Diff_95CI_upper <- incona$compare.fixed$upper
    NMA <- incona$fixed$TE
    pValue <- incona$compare.fixed$p
  }

  return(data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower,
                    Diff_95CI_upper, pValue))

}
