#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param is_direct Contributions from function `CalculateDirectness()`.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param covariate_symbol The selected symbol for displaying covariates. Defaults to "circle open".
#' @param covariate_symbol_size Size of the covariate symbols. Defaults to 10.
#'
#' @return Created ggplot2 object.
CreateIndirectCovariatePlot <- function(
    model_output,
    treatment_df,
    comparators,
    is_direct,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    covariate_symbol = "circle open",
    covariate_symbol_size = 10) {

  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names
  
  # Set up basic plot
  plot <- .SetupIndirectCovariatePlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators)
  )
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    plot <- .PlotIndirectCovariateCircles(plot, model_output, treatment_df, reference, ghosts, is_direct, covariate_symbol, covariate_symbol_size, ghosted = TRUE)
  }
  
  if (length(comparators) > 0) {
    plot <- .PlotIndirectCovariateCircles(plot, model_output, treatment_df, reference, comparators, is_direct, covariate_symbol, covariate_symbol_size)
  }
  
  if (include_covariate) {
    plot <- plot +
      geom_vline(
        xintercept = model_output$covariate_value,
        color = "black"
      )
  }
  
  return(plot)
}

#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all otherc omparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#'
#' @return Created ggplot2 object.
.SetupIndirectCovariatePlot <- function(reference, comparators, include_ghosts) {
  # Set up basic plot
  plot <- ggplot() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#777777", fill = NA, linewidth = 2),
      
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14)
    ) +
    ylab("Indirect\nEvidence")
  
  plot <- SetupRegressionPlotColours(
    plot = plot,
    comparators = comparators,
    include_ghosts = include_ghosts,
    include_credible = FALSE
  )
  
  return(plot)
}

#' Plot the contribution circles on the plot.
#'
#' @param plot object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param is_direct Contributions from function `CalculateDirectness()`.
#' @param covariate_symbol The selected symbol for displaying covariates. Defaults to "circle open".
#' @param covariate_symbol_size Size of the covariate symbols. Defaults to 10.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotIndirectCovariateCircles <- function(plot, model_output, treatment_df, reference, comparators, is_direct, covariate_symbol = "circle open", covariate_symbol_size = 10, ghosted = FALSE) {
  contributions = .FindIndirectRegressionCovariates(model_output, reference, comparators, is_direct)

  if (nrow(contributions) == 0) {
    return(plot)
  }
  
  contributions$Treatment <- sapply(contributions$Treatment, function(treatment) { treatment_df$RawLabel[treatment_df$Label == treatment] })
  
  if (ghosted) {
    contributions$Treatment <- rep(regression_ghost_name, length(contributions$Treatment))
  }
  
  #Give each circle a distinct y-value so they can all be seen
  contributions$y_val <- seq(from = -1, to = 1, length.out = length(contributions[[1]]))
  
  plot <- plot +
    geom_point(
      data = contributions,
      mapping = aes(
        x = covariate_value,
        y = y_val,
        color = Treatment,
        stroke = 1.5,
      ),
      shape = covariate_symbol,
      size = covariate_symbol_size,
      show.legend = FALSE,
    ) +
    ylim(-10, 10)
  
  return(plot)
}

#' Find the contributions to the regression analysis.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param is_direct Contributions from function `CalculateDirectness()`.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - contribution: Size of contribution for this study.
.FindIndirectRegressionCovariates <- function(model_output, reference, comparator, is_direct) {
  treatments <- c()
  covariate_values <- c()
  contributions <- c()
  
  for (treatment in comparator) {
    for (study in row.names(is_direct$is_direct)) {
      contribution_is_direct <- is_direct$is_direct[study, treatment]
      
      if (is.na(contribution_is_direct) || contribution_is_direct) {
        next
      }
      
      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, is_direct$covariate_value[study])
      contributions <- c(contributions, contribution_is_direct)
    }
  }

  return(
    data.frame(
      Treatment = treatments,
      covariate_value = covariate_values,
      contribution = contributions
    )
  )
}
