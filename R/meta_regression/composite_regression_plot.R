
#' Create a composite meta-regression plot which comprises plots showing direct and indirect evidence.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_type Reactive type of outcome (OR, RR, RD, MD or SD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contribution TRUE if study contribution should be displayed as circles. Defaults to TRUE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
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
    outcome_type,
    comparators,
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
    model_output = model_output,
    treatment_df = treatment_df,
    outcome_type = outcome_type,
    comparators = comparators,
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

#' Example for the meta-regression main plot.
#'
#' @return Created ggplot2 object.
.MetaRegressionCompositePlotExample <- function() {
  data <- read.csv("tests/testthat/Cont_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data), reference_treatment = "the Little")
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  model <- RunCovariateModel(data, wrangled_treatment_list, "Continuous", 'MD', "covar.age", "age", 'random', 'unrelated', "the_Little")
  model_output <<- CovariateModelOutput(model, 98)
  
  plot <- CreateCompositeMetaRegressionPlot(
    model_output = model_output,
    treatment_df = wrangled_treatment_list,
    comparators = c("the_Butcher", "the_Dung_named"),
    contribution_type = "percentage",
    include_covariate = TRUE,
    include_ghosts = TRUE,
    include_extrapolation = TRUE,
    include_confidence = TRUE,
    confidence_opacity = 0.2,
    contribution_multiplier = 5.0
  )
  
  return(plot)
}
