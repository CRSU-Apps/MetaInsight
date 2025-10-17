#' @title baseline_comparison
#' Creates a table of comparisons of all treatment pairs for baseline risk models
#'
#' @param model list. Object created by `baseline_model()`
#' @inheritParams common_params
#' @return Relative effects table created by `gemtc::relative.effect.table()`
#' @export
baseline_comparison <- function(model, logger = NULL){

  if (!inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model()")
  }

  median_ci_table <- bnma::relative.effects.table(model$mtcResults, summary_stat = "ci")
  BaselineRiskRelativeEffectsTable(median_ci_table)
}

#' Puts a relative effects table from bnma into gemtc format
#'
#' @param median_ci_table Output from bnma::relative.effects.table(, summary_stat = "ci")
#' @return A relative effects table in the same format as from gemtc.
BaselineRiskRelativeEffectsTable <- function(median_ci_table) {
  #Entries in the input table are in the form "[lower_ci,median,upper_ci]" (no spaces)

  #The dimensions of the (square) table
  dim_median <- nrow(median_ci_table)
  #Create matrices to store the lower_ci, median and upper_ci separately
  lower_ci <- matrix(nrow = dim_median, ncol = dim_median)
  median_br <- matrix(nrow = dim_median, ncol = dim_median)
  upper_ci <- matrix(nrow = dim_median, ncol = dim_median)

  for (row in 1:dim_median) {
    for (col in 1:dim_median) {
      #Extract lower_ci, median and upper_ci
      interval <- round(
        as.numeric(
          stringr::str_extract_all(
            string = median_ci_table[row, col],
            pattern = "[-0-9\\.]+")[[1]]
        ),
        digits = 2
      )
      lower_ci[row, col] <- interval[1]
      median_br[row, col] <- interval[2]
      upper_ci[row, col] <- interval[3]
    }
  }

  #Paste into the format "median (lower_ci, upper_ci)"
  median_ci_table_new <- matrix(paste0(median_br, " (", lower_ci, ", ", upper_ci, ")"), nrow = dim_median)
  diag(median_ci_table_new) <- rownames(median_ci_table)
  rownames(median_ci_table_new) <- rownames(median_ci_table)
  colnames(median_ci_table_new) <- colnames(median_ci_table)

  return(median_ci_table_new)
}
