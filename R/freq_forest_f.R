#' Produce a forest plot and annotation
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param reference_treatment character. The reference treatment of the dataset
#' @param model_type character. Type of model to fit, either `random` or `fixed`
#' @param outcome_measure character. Outcome measure of the dataset. Either
#' `OR`, `RR` or `RD` when `outcome` is `Binary` or `MD` or `SMD` when
#' `outcome` is `Continuous`
#' @param xmin numeric. Minimum x-axis limit.
#' @param xmax numeric. Maximum x-axis limit.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#'
#' @return List containing:
#'  \item{plot}{function. Forest plot created using `meta::forest()`}
#'  \item{annotation}{character. Annotation to add to the plot}
#'  \item{height}{numeric. Plot height in inches}
#'  \item{width}{numeric. Plot width in inches}
#'
#' @export
freq_forest <- function(freq, reference_treatment, model_type, outcome_measure, xmin, xmax, logger = NULL) {

  check_param_classes(c("freq", "reference_treatment", "model_type", "outcome_measure", "xmin", "xmax"),
                      c("list", "character", "character", "character", "numeric", "numeric"), logger)

  if (!model_type %in% c("fixed", "random")){
    logger |> writeLog(type = "error", "model_type must be 'fixed' or 'random'")
    return()
  }

  if (!outcome_measure %in% c("OR", "RR", "RD", "MD", "SMD")){
    logger |> writeLog(type = "error", "outcome_measure must be 'OR', 'RR', 'RD', 'MD' or 'SMD'")
    return()
  }

  plot <- function(){meta::forest(freq$net1, reference.group = reference_treatment, pooled = model_type, xlim = c(xmin, xmax))}
  annotation <- forest_annotation(freq, model_type, outcome_measure)
  n_treatments <- length(levels(freq$net1$data$treat1))
  height <- forest_height(n_treatments, title = TRUE, annotation = TRUE)
  width <- forest_width(max(nchar(freq$lstx)))

  return(list(plot = plot,
              annotation = annotation,
              height = height,
              width = width
              ))
}


#' Extract the minimum and maximum confidence intervals from the summary produced by netmeta
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param outcome character. `Binary` or `Continuous`
#'
#' @return List containing:
#'  \item{xmin}{numeric. Minimum confidence interval}
#'  \item{xmax}{numeric. Maximum confidence interval}
#' @export
extract_ci <- function(freq, outcome){

  # store the result of print(freq$net1) produced by netmeta
  net1_summary <- capture.output(freq$net1)

  # extract the treatment estimate lines
  first_line <- grep("Treatment estimate", net1_summary ) + 2
  last_line <- first_line + length(levels(freq$net1$data$treat1)) - 1
  treatment_estimates <- net1_summary[first_line:last_line]

  # extract the square brackets and then the values inside
  square_brackets <- unlist(regmatches(treatment_estimates, gregexpr("\\[([-0-9.; ]+)\\]", treatment_estimates)))
  ci_values <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", square_brackets), ";")))

  if (outcome == "Continuous"){
    # add a 20% buffer to the CIs and round to 0.1
    xmin <- round(min(ci_values) * 1.2, 1)
    xmax <- round(max(ci_values) * 1.2, 1)
  }
  if (outcome == "Binary"){
    xmin <- round(min(ci_values) * 1.2, 1)
    # prevent errors
    if (xmin == 0){
      xmin = 0.01
    }
    xmax <- round(max(ci_values) * 1.2, 1)
  }

  return(list(xmin = xmin,
              xmax = xmax))
}

#' Creates the text to be displayed underneath the forest plots, with between-study SD, number of studies and number of treatments.
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param model_type "fixed" or "random".
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @return Text as described above.
forest_annotation <- function(freq, model_type, outcome_measure) {

  tau <- round(freq$net1$tau, 2)
  k <- freq$net1$k
  n <- freq$net1$n

  if (model_type == "random") {
    if (outcome_measure == "OR") {
      output_text <- paste0("Between-study standard deviation (log-odds scale): ", tau,
                           "\n Number of studies: ", k,
                           ", Number of treatments: ", n)
    } else if (outcome_measure == "RR") {
      output_text <- paste0("Between-study standard deviation (log probability scale): ", tau,
                           "\n Number of studies: ", k,
                           ", Number of treatments: ", n)
    } else if(outcome_measure %in% c("MD", "SMD", "RD")) {
      output_text <- paste0("Between-study standard deviation: ", tau,
                           "\n Number of studies: ", k,
                           ", Number of treatments: ", n)
    } else {
      stop("outcome_measure must be one of 'MD', 'SMD', 'OR', 'RR', 'RD'")
    }
  } else if (model_type == "fixed") {
    if (outcome_measure == "OR") {
      output_text <- paste0("Between-study standard deviation (log-odds scale) set at 0. \n Number of studies: ", k,
                           ", Number of treatments:", n)
    }
    else if (outcome_measure == "RR") {
      output_text <- paste0("Between-study standard deviation (log probability scale) set at 0. \n Number of studies: ", k,
                           ", Number of treatments: ", n)}
    else if(outcome_measure %in% c("MD", "SMD", "RD")) {
      output_text <- paste0("Between-study standard deviation set at 0. \n Number of studies: ", k,
                           ", Number of treatments: ", n)
    } else {
      stop("outcome_measure must be one of 'MD', 'SMD', 'OR', 'RR', 'RD'")
    }
  } else {
    stop("model_type must be 'fixed' or 'random'")
  }

  return(output_text)
}
