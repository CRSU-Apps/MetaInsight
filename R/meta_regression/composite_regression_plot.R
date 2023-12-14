
#' Create a composite meta-regression plot which comprises plots showing direct and indirect evidence.
#'
#' @param model GEMTC model result object.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param covariate_value The value of the covariate to plot as a vertical line. NULL if not to be plotted.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
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
    model,
    treatment_df,
    reference,
    comparators,
    covariate_value,
    contribution_type,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0,
    legend_position = "BR") {
  
  direct_plot <- CreateMainRegressionPlot(
    model = model,
    treatment_df = treatment_df,
    reference = reference,
    comparators = comparators,
    covariate_value = covariate_value,
    contribution_type = contribution_type,
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
    model = model,
    treatment_df = treatment_df,
    reference = reference,
    comparators = comparators,
    covariate_value = covariate_value,
    contribution_type = contribution_type,
    include_ghosts = include_ghosts,
    contribution_multiplier = contribution_multiplier
  )
  
  x_range_1 <- ggplot_build(direct_plot)$layout$panel_params[[1]]$x.range
  x_range_2 <- ggplot_build(indirect_plot)$layout$panel_params[[1]]$x.range
  
  x_min = min(x_range_1[1], x_range_2[1])
  x_max = max(x_range_1[2], x_range_2[2])
  
  direct_plot <- direct_plot + coord_cartesian(xlim = c(x_min, x_max)) 
  indirect_plot <- indirect_plot + coord_cartesian(xlim = c(x_min, x_max)) 
  
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
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  model <- RunCovariateModel(data, wrangled_treatment_list, "Continuous", 'MD', "covar.age", "age", 'random', 'unrelated', "the_Little")
  
  plot <- CreateCompositeMetaRegressionPlot(
    model = model,
    treatment_df = wrangled_treatment_list,
    reference = "the_Little",
    comparators = c("the_Butcher", "the_Dung_named"),
    covariate_value = 4.16,
    contribution_type = "percentage",
    include_ghosts = TRUE,
    include_extrapolation = TRUE,
    include_confidence = TRUE,
    confidence_opacity = 0.2,
    contribution_multiplier = 5.0
  )
  
  return(plot)
}
