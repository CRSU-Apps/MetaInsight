#' Produce a frequentist forest plot and annotation using `meta::forest()`
#'
#' @param xmin numeric. Minimum x-axis limit.
#' @param xmax numeric. Maximum x-axis limit.
#' @param title character. Title for the plot.
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
freq_forest <- function(configured_data, xmin, xmax, title, logger = NULL) {

  check_param_classes(c("configured_data", "xmin", "xmax", "title"),
                      c("configured_data", "numeric", "numeric", "character"), logger)

  n_treatments <- length(configured_data$freq$lstx)
  annotation <- forest_annotation(configured_data$freq, configured_data$effects, configured_data$outcome_measure)
  height <- forest_height(n_treatments, title = TRUE, annotation = TRUE)
  width <- forest_width(max(nchar(configured_data$freq$lstx)))

  svg <- svglite::xmlSVG({
   meta::forest(configured_data$freq$net1,
                reference.group = configured_data$reference_treatment,
                pooled = configured_data$effects,
                xlim = c(xmin, xmax))
   grid::grid.text(title, 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
   grid::grid.text(annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))
  },
  height = height,
  width = width,
  web_fonts = list(
    Arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)
}


#' Extract the minimum and maximum confidence intervals from the summary produced by netmeta
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param outcome character. `binary` or `continuous`
#'
#' @return List containing:
#'  \item{xmin}{numeric. Minimum confidence interval}
#'  \item{xmax}{numeric. Maximum confidence interval}
#' @export
extract_ci <- function(freq, outcome){

  # store the result of print(freq$net1) produced by netmeta
  net1_summary <- utils::capture.output(freq$net1)

  # extract the treatment estimate lines
  first_line <- grep("Treatment estimate", net1_summary ) + 2
  last_line <- first_line + length(levels(freq$net1$data$treat1)) - 1
  treatment_estimates <- net1_summary[first_line:last_line]

  # extract the square brackets and then the values inside
  square_brackets <- unlist(regmatches(treatment_estimates, gregexpr("\\[([-0-9.; ]+)\\]", treatment_estimates)))
  ci_values <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", square_brackets), ";")))

  # add a 20% buffer to the CIs and round to 0.1
  xmin <- round(min(ci_values) - (min(ci_values) * 0.2), 1)
  xmax <- round(max(ci_values) + (max(ci_values) * 0.2), 1)

  # prevent errors
  if (outcome == "binary" && xmin == 0){
      xmin <- 0.01
  }

  return(list(xmin = xmin,
              xmax = xmax))
}

#' Creates the text to be displayed underneath the forest plots, with between-study SD, number of studies and number of treatments.
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param effects "fixed" or "random".
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @return Text as described above.
forest_annotation <- function(freq, effects, outcome_measure) {

  tau <- round(freq$net1$tau, 2)
  k <- freq$net1$k
  n <- freq$net1$n

  if (effects == "random") {
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
  } else if (effects == "fixed") {
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
    stop("effects must be 'fixed' or 'random'")
  }

  return(output_text)
}
