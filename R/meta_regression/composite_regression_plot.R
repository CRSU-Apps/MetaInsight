
# Zero-width space before "Other" to pin it to the first item in the list
regression_ghost_name = "\u200BOther"

#' Create a composite meta-regression plot which comprises plots showing direct and indirect evidence.
#'
#' @param data Data from which to find covariate ranges.
#' @param covariate_title Title of the covariate column in the data
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_type Reactive type of outcome (OR, RR, RD, MD or SD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contributions TRUE if the contributions should be plotted as a circle for each study. Defaults to TRUE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
CreateCompositeMetaRegressionPlot <- function(
    data,
    covariate_title,
    model_output,
    treatment_df,
    outcome_type,
    comparators,
    contribution_matrix,
    contribution_type,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0,
    legend_position = "BR") {
  
  direct_plot <- CreateMainRegressionPlot(
    data = data,
    covariate_title = covariate_title,
    model_output = model_output,
    treatment_df = treatment_df,
    outcome_type = outcome_type,
    comparators = comparators,
    contribution_matrix = contribution_matrix,
    contribution_type = contribution_type,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    include_extrapolation = include_extrapolation,
    include_confidence = include_confidence,
    confidence_opacity = confidence_opacity,
    include_contributions = include_contributions,
    contribution_multiplier = contribution_multiplier,
    legend_position = legend_position
  )
  
  if (!include_contributions) {
    return(direct_plot)
  }
  
  indirect_plot <- CreateIndirectContributionPlot(
    model_output = model_output,
    treatment_df = treatment_df,
    comparators = comparators,
    contribution_matrix = contribution_matrix,
    contribution_type = contribution_type,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    contribution_multiplier = contribution_multiplier
  )
  
  # Find the x-axis ranges of the 2 plots
  x_range_1 <- ggplot_build(direct_plot)$layout$panel_params[[1]]$x.range
  x_range_2 <- ggplot_build(indirect_plot)$layout$panel_params[[1]]$x.range
  
  # Find the largest range covered by either plot
  x_min = min(x_range_1[1], x_range_2[1])
  x_max = max(x_range_1[2], x_range_2[2])
  
  # Scale both plots to cover the full x-axis range
  direct_plot <- direct_plot + coord_cartesian(xlim = c(x_min, x_max)) 
  indirect_plot <- indirect_plot + coord_cartesian(xlim = c(x_min, x_max)) 
  
  # Create composite plot by placing the indirect plot atop the direct plot
  plot <- ggpubr::ggarrange(
    indirect_plot, direct_plot,
    heights = c(1, 4),
    align = "v",
    ncol = 1
  )
  
  return(plot)
}
