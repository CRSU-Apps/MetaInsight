#' @param freq list. NMA results created by freq_wrap().
#' @param model_type  character. "fixed" or "random".
#' @return Dataframe of inconsistency data:
#'  - 'Comparison': Treatment comparison.
#'  - 'No.Studies': Number of studies.
#'  - 'NMA': NMA treatment effect estimate.
#'  - 'Direct': Direct treatment effect estimate.
#'  - 'Indirect': Indirect treatment effect estimate.
#'  - 'Difference': Difference between treatment effects.
#'  - 'Diff_95CI_lower': 2.5% limit of difference in treatment effects.
#'  - 'Diff_95CI_upper': 97.5% limit of difference in treatment effects.
#'  - 'pValue': p-value for test of "difference in treatment effects == 0".
#' @export
freq_inconsistent <- function(freq, model_type, logger = NULL) {

  check_param_classes(c("freq", "model_type"),
                      c("list", "character"), logger)

  if (!model_type %in% c("fixed", "random")){
    logger %>% writeLog(type = "error", "model_type must be 'fixed' or 'random'")
    return()
  }

  incona <- netmeta::netsplit(freq$net1)

  Comparison <- incona$comparison
  No.Studies <- as.integer(incona$k)

  if (model_type == "random") {
    Direct <- incona$direct.random$TE
    Indirect <- incona$indirect.random$TE
    Difference <- incona$compare.random$TE
    Diff_95CI_lower <- incona$compare.random$lower
    Diff_95CI_upper <- incona$compare.random$upper
    NMA <- incona$random$TE
    pValue <- incona$compare.random$p
  }
  if (model_type == "fixed") {
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
