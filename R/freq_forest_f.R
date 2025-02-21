#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param reference_treatment character. The reference treatment of the dataset
#' @param model_type character. Type of model to fit, either `random` or `fixed`
#' @param xmin numeric. Minimum x-axis limit.
#' @param xmax numeric. Maximum x-axis limit.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#'
#' @return Forest plot created using metafor::forest().
#' @export
freq_forest <- function(freq, reference_treatment, model_type, xmin, xmax, logger = NULL) {
  check_param_classes(c("freq", "reference treatment", "model_type", "xmin", "xmax"),
                      c("list", "character", "character", "numeric", "numeric"), logger)

  return(metafor::forest(freq$net1, reference.group = reference_treatment, pooled = model_type, xlim = c(xmin, xmax)))
}


#' Extract the minimum and maximum confidence intervals from the summary produced by netmeta
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param treatment_df dataframe. Treatments and their IDs
#'
#' @return List containing:
#'  \item{is_data_valid}{logical. Whether the data is valid}
#'  \item{is_data_uploaded}{logical. Whether the data is uploaded}
#' @export
extract_ci <- function(freq, treatment_df){

  # store the result of print(freq$net1) produced by netmeta
  net1_summary <- capture.output(freq$net1)

  # extract the treatment estimate lines
  first_line <- grep("Treatment estimate", net1_summary ) + 2
  last_line <- first_line + nrow(treatment_df) - 1
  treatment_estimates <- net1_summary[first_line:last_line]

  # extract the square brackets and then the values inside
  square_brackets <- unlist(regmatches(treatment_estimates, gregexpr("\\[([-0-9.; ]+)\\]", treatment_estimates)))
  ci_values <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", square_brackets), ";")))

  # add a 20% buffer to the CIs
  xmin <- min(numeric_values) * 1.2
  xmax <- max(numeric_values) * 1.2

  return(list(xmin = xmin, xmax = xmax))
}
