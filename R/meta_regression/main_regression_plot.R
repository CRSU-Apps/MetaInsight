
#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD, MD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution, used to calculate sizes for the study contribution circles.
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contributions TRUE if the contributions should be plotted as a circle for each study. Defaults to TRUE.
#' @param contribution_multiplier Multiplication factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#' Defaults to "BR"
#'
#' @return Created ggplot2 object.
CreateMainRegressionPlot <- function(
    model_output,
    treatment_df,
    outcome_measure,
    comparators,
    contribution_matrix,
    contribution_type,
    confidence_regions,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0,
    legend_position = "BR") {
  
  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names
  
  # Set up basic plot
  plot <- .SetupMainRegressionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    outcome_measure = outcome_measure,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators),
    confidence_opacity = confidence_opacity,
    legend_position = legend_position
  )
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    
    if (include_contributions) {
      plot <- .PlotDirectContributionCircles(plot, model_output, treatment_df, reference, ghosts, contribution_matrix, contribution_type, contribution_multiplier, ghosted = TRUE)
    }
    plot <- .PlotRegressionLines(plot, model_output, contribution_matrix, treatment_df, reference, ghosts, include_extrapolation, ghosted = TRUE)
  }
  
  if (length(comparators) > 0) {
    if (include_confidence) {
      plot <- .PlotConfidenceRegions(plot, confidence_regions, comparators, confidence_opacity)
    }
    if (include_contributions) {
      plot <- .PlotDirectContributionCircles(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier)
    }
    plot <- .PlotRegressionLines(plot, model_output, contribution_matrix, treatment_df, reference, comparators, include_extrapolation)
  }
  
  # Plot a vertical line at the covariate value
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
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD, MD)
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
.SetupMainRegressionPlot <- function(reference, comparators, outcome_measure, include_ghosts, confidence_opacity, legend_position) {
  # Set up basic plot
  plot <- ggplot() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#777777", fill = NA, linewidth = 2),
      
      axis.line = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      
      legend.position = c(
        ifelse(grepl("L", legend_position), .01, .99),
        ifelse(grepl("B", legend_position), .02, .99)
      ),
      legend.justification = c(
        ifelse(grepl("L", legend_position), "left", "right"),
        ifelse(grepl("B", legend_position), "bottom", "top")
      ),
      
      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(colour = "black", linewidth = 0.5, fill = "#ffffffaa"),
      legend.text = element_text(size = 12)
    ) +
    xlab("Covariate Value") +
    ylab(glue::glue("Relative Effect vs {reference} ({outcome_measure})"))
  
  # Log scale for OR & RR
  if (outcome_measure %in% c("OR", "RR")) {
    plot <- plot + 
      scale_y_continuous(labels = function(x) { signif(exp(x), digits = 2) })
  }
  
  plot <- SetupRegressionPlotColours(
    plot = plot,
    comparators = comparators,
    include_ghosts = include_ghosts,
    include_confidence = TRUE,
    confidence_opacity = confidence_opacity
  )
  
  return(plot)
}

#' Plot the confidence regions and intervals on the plot.
#'
#' @param plot object to which to add elements.
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param comparators Vector of names of comparison treatments to plot.
#'
#' @return The modified ggplot2 object.
.PlotConfidenceRegions <- function(plot, confidence_regions, comparators, confidence_opacity) {
  regions <- .FormatRegressionConfidenceRegion(confidence_regions$regions, comparators)
  intervals <- .FormatRegressionConfidenceRegion(confidence_regions$intervals, comparators)
  
  plot <- plot +
    geom_ribbon(
      data = regions,
      mapping = aes(
        x = covariate_value,
        ymin = y_min,
        ymax = y_max,
        fill = Treatment
      ),
      show.legend = FALSE
    ) +
    geom_linerange(
      data = intervals,
      mapping = aes(
        x = covariate_value,
        ymin = y_min,
        ymax = y_max,
        color = Treatment
      ),
      linewidth = 2,
      alpha = confidence_opacity,
      show.legend = FALSE
    )
  
  return(plot)
}

#' Plot the contribution circles for direct evidence on the plot.
#'
#' @param plot ggplot2 object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Multiplication factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotDirectContributionCircles <- function(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindDirectRegressionContributions(model_output, reference, comparators, contribution_matrix, contribution_type)
  
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
        y = relative_effect,
        color = Treatment,
        stroke = 1.5
      ),
      shape = 1,
      size = contributions$contribution * contribution_multiplier,
      show.legend = FALSE
    )
  
  return(plot)
}

#' Plot the regression lines on the plot.
#'
#' @param plot ggplot2 object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param extrapolate TRUE if regression lines should be extrapolated beyond the range of the data. These will be plotted as dashed lines.
#' Defaults to FALSE.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotRegressionLines <- function(plot, model_output, contribution_matrix, treatment_df, reference, comparators, extrapolate, ghosted = FALSE) {
  # Create data frame
  lines = data.frame(
    Treatment = sapply(comparators, function(comparator) { treatment_df$RawLabel[treatment_df$Label == comparator] }),
    intercept = model_output$intercepts[comparators],
    slope = model_output$slopes[comparators],
    start_x = model_output$covariate_min[comparators],
    end_x = model_output$covariate_max[comparators]
  )
  
  if (ghosted) {
    lines$Treatment <- rep(regression_ghost_name, length(lines$Treatment))
  }
  
  if (extrapolate) {
    # Dashed lines outside of data ranges
    plot <- plot +
      geom_abline(
        data = lines,
        mapping = aes(
          intercept = intercept,
          slope = slope,
          color = Treatment
        ),
        linewidth = 1,
        linetype = "dashed",
        show.legend = FALSE
      )
  }
  
  # Solid lines within data ranges
  plot <- plot +
    geom_segment(
      data = lines,
      mapping = aes(
        x = start_x,
        y = intercept + slope * start_x,
        xend = end_x,
        yend = intercept + slope * end_x,
        color = Treatment
      ),
      linewidth = 1.2,
      show.legend = !ghosted
    )
  
  return(plot)
}

#' Find the contributions to the regression analysis.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution to find.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - relative_effect Relative effect for this study.
#' - contribution: Size of contribution for this study.
.FindDirectRegressionContributions <- function(model_output, reference, comparator, contribution_matrix, contribution_type) {
  treatments <- c()
  covariate_values <- c()
  relative_effects <- c()
  contributions <- c()
  
  for (treatment in comparator) {
    for (study in row.names(contribution_matrix$direct)) {
      direct_contribution <- contribution_matrix$direct[study, treatment]
      
      if (is.na(direct_contribution)) {
        next
      }
      
      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, contribution_matrix$covariate_value[study])
      relative_effects <- c(relative_effects, contribution_matrix$relative_effect[study, treatment])
      contributions <- c(contributions, direct_contribution)
    }
  }
  
  return(
    data.frame(
      Treatment = treatments,
      covariate_value = covariate_values,
      relative_effect = relative_effects,
      contribution = contributions
    )
  )
}

#' Format the confidence regions for the regression analysis into a plottable data frame.
#'
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param comparator Name of comparison treatment for which to find the contributions.
#'
#' @return Data frame containing contribution details. Each row represents a confidence interval at a specific covariate value, for a given treatment. Columns are:
#' - Treatment: The treatment for which this confidence interval relates.
#' - covariate_value: Value of the covariate for this interval
#' - y_min Relative effect of the lower end of this interval
#' - y_max: Relative effect of the upper end of this interval
.FormatRegressionConfidenceRegion <- function(confidence_regions, comparator) {
  
  treatments <- c()
  covariate_values <- c()
  y_mins <- c()
  y_maxs <- c()
  
  for (treatment_name in comparator) {
    treatment_covariate_values <- confidence_regions[[treatment_name]]$cov_value
    treatment_y_mins <- confidence_regions[[treatment_name]]$lower
    treatment_y_maxs <- confidence_regions[[treatment_name]]$upper
    
    treatments <- c(treatments, rep(treatment_name, length(treatment_covariate_values)))
    covariate_values <- c(covariate_values, treatment_covariate_values)
    y_mins <- c(y_mins, treatment_y_mins)
    y_maxs <- c(y_maxs, treatment_y_maxs)
  }
  
  confidence_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    y_min = y_mins,
    y_max = y_maxs
  )
  
  return(confidence_df)
}
