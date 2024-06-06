#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#'
#' @return Created ggplot2 object.
CreateIndirectContributionPlot <- function(
    model_output,
    treatment_df,
    comparators,
    contribution_matrix,
    contribution_type,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    contribution_multiplier = 1.0) {
  
  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names
  
  # Set up basic plot
  plot <- .SetupIndirectContributionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators)
  )
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    plot <- .PlotIndirectContributionCircles(plot, model_output, treatment_df, reference, ghosts, contribution_matrix, contribution_type, contribution_multiplier, ghosted = TRUE)
  }
  
  if (length(comparators) > 0) {
    plot <- .PlotIndirectContributionCircles(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier)
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
.SetupIndirectContributionPlot <- function(reference, comparators, include_ghosts) {
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
    include_confidence = FALSE
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
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotIndirectContributionCircles <- function(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindIndirectRegressionContributions(model_output, reference, comparators, contribution_matrix, contribution_type)
  
  if (nrow(contributions) == 0) {
    return(plot)
  }
  
  contributions$Treatment <- sapply(contributions$Treatment, function(treatment) { treatment_df$RawLabel[treatment_df$Label == treatment] })
  
  if (ghosted) {
    contributions$Treatment <- rep(regression_ghost_name, length(contributions$Treatment))
  }
  
  plot <- plot +
    geom_point(
      data = contributions,
      mapping = aes(
        x = covariate_value,
        y = 0,
        color = Treatment,
        stroke = 1.5
      ),
      shape = 1,
      size = contributions$contribution * contribution_multiplier,
      show.legend = FALSE
    )
  
  return(plot)
}

#' Find the contributions to the regression analysis.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param contribution_type Type of contribution to find.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - contribution: Size of contribution for this study.
.FindIndirectRegressionContributions <- function(model_output, reference, comparator, contribution_matrix, contribution_type) {
  treatments <- c()
  covariate_values <- c()
  contributions <- c()
  
  for (treatment in comparator) {
    for (study in row.names(contribution_matrix$indirect)) {
      indirect_contribution <- contribution_matrix$indirect[study, treatment]
      
      if (is.na(indirect_contribution)) {
        next
      }
      
      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, contribution_matrix$covariate_value[study])
      contributions <- c(contributions, indirect_contribution)
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
