
# Zero-width space before "Other" to pin it to the first item in the list
regression_ghost_name = "\"Other\""

#' Create a composite meta-regression plot which comprises plots showing direct and indirect evidence.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD or MD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param directness Contributions from function `CalculateDirectness()`.
#' @param credible_regions List of credible region data frames from function `CalculateCredibleRegions()`.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_credible TRUE if the credible regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param credible_opacity The opacity of the credible regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param covariate_symbol The selected symbol for displaying covariates. Defaults to "circle open".
#' @param covariate_symbol_size Size of the covariate symbols. Defaults to 10.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
CreateCompositeMetaRegressionPlot <- function(
    model_output,
    treatment_df,
    outcome_measure,
    comparators,
    directness,
    credible_regions,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_credible = FALSE,
    credible_opacity = 0.2,
    covariate_symbol = "circle open",
    covariate_symbol_size = 10,
    legend_position = "BR") {

  direct_plot <- CreateMainRegressionPlot(
    model_output = model_output,
    treatment_df = treatment_df,
    outcome_measure = outcome_measure,
    comparators = comparators,
    directness = directness,
    credible_regions = credible_regions,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    include_extrapolation = include_extrapolation,
    include_credible = include_credible,
    credible_opacity = credible_opacity,
    covariate_symbol = covariate_symbol,
    covariate_symbol_size = covariate_symbol_size,
    legend_position = legend_position
  )

  if (covariate_symbol == "none") {
    return(direct_plot)
  }
  
  indirect_plot <- CreateIndirectCovariatePlot(
    model_output = model_output,
    treatment_df = treatment_df,
    comparators = comparators,
    directness = directness,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    covariate_symbol = covariate_symbol,
    covariate_symbol_size = covariate_symbol_size
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
