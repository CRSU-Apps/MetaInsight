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
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @export
metaregression_plot <- function(
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
    return(
      svglite::xmlSVG({
        print(direct_plot)
      },
      height = 11,
      width = 16
      ) |> crop_svg()
    )
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

  svglite::xmlSVG({
    print(plot)
  },
  height = 11,
  width = 16
  ) |> crop_svg()
}

regression_ghost_name = "\"Other\""

#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD, MD)
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
#' Defaults to "BR"
#'
#' @return Created ggplot2 object.
CreateMainRegressionPlot <- function(
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

  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names

  # Set up basic plot
  plot <- .SetupMainRegressionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    outcome_measure = outcome_measure,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators),
    credible_opacity = credible_opacity,
    legend_position = legend_position
  )

  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]

    if (covariate_symbol != "none") {
      plot <- .PlotDirectCovariateCircles(plot, model_output, treatment_df, reference, ghosts, directness, covariate_symbol, covariate_symbol_size, ghosted = TRUE)
    }
    plot <- .PlotRegressionLines(plot, model_output, treatment_df, reference, ghosts, include_extrapolation, ghosted = TRUE)
  }

  if (length(comparators) > 0) {
    if (include_credible) {
      plot <- .PlotCredibleRegions(plot, credible_regions, comparators, credible_opacity)
    }
    if (covariate_symbol != "none") {
      plot <- .PlotDirectCovariateCircles(plot, model_output, treatment_df, reference, comparators, directness, covariate_symbol, covariate_symbol_size)
    }
    plot <- .PlotRegressionLines(plot, model_output, treatment_df, reference, comparators, include_extrapolation)
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
#' @param credible_opacity The opacity of the credible regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
.SetupMainRegressionPlot <- function(reference, comparators, outcome_measure, include_ghosts, credible_opacity, legend_position) {

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
    include_credible = TRUE,
    credible_opacity = credible_opacity
  )

  return(plot)
}

#' Plot the credible regions and intervals on the plot.
#'
#' @param plot object to which to add elements.
#' @param credible_regions List of credible region data frames from function `CalculateCredibleRegions()`.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param credible_opacity The opacity of the credible regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#'
#' @return The modified ggplot2 object.
.PlotCredibleRegions <- function(plot, credible_regions, comparators, credible_opacity = 0.2) {

  regions <- .FormatRegressionCredibleRegion(credible_regions$regions, comparators)
  intervals <- .FormatRegressionCredibleRegion(credible_regions$intervals, comparators)

  plot <- plot +
    ggplot2::geom_ribbon(
      data = regions,
      mapping = aes(
        x = .data$covariate_value,
        ymin = .data$y_min,
        ymax = .data$y_max,
        fill = .data$Treatment
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_linerange(
      data = intervals,
      mapping = aes(
        x = .data$covariate_value,
        ymin = .data$y_min,
        ymax = .data$y_max,
        color = .data$Treatment
      ),
      linewidth = 2,
      alpha = credible_opacity,
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
#' @param directness Contributions from function `CalculateDirectness()`.
#' @param covariate_symbol The selected symbol for displaying covariates. Defaults to "circle open".
#' @param covariate_symbol_size Size of the covariate symbols. Defaults to 10.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotDirectCovariateCircles <- function(plot, model_output, treatment_df, reference, comparators, directness, covariate_symbol = "circle open", covariate_symbol_size = 10, ghosted = FALSE) {

  contributions = .FindDirectRegressionContributions(model_output, reference, comparators, directness)

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
        x = .data$covariate_value,
        y = .data$relative_effect,
        color = .data$Treatment,
        stroke = 1.5
      ),
      shape = covariate_symbol,
      size = covariate_symbol_size,
      show.legend = FALSE
    )

  return(plot)
}

#' Plot the regression lines on the plot.
#'
#' @param plot ggplot2 object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param extrapolate TRUE if regression lines should be extrapolated beyond the range of the data. These will be plotted as dashed lines.
#' Defaults to FALSE.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotRegressionLines <- function(plot, model_output, treatment_df, reference, comparators, extrapolate, ghosted = FALSE) {

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
          intercept = .data$intercept,
          slope = .data$slope,
          color = .data$Treatment
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
        x = .data$start_x,
        y = .data$intercept + .data$slope * .data$start_x,
        xend = .data$end_x,
        yend = .data$intercept + .data$slope * .data$end_x,
        color = .data$Treatment
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
#' @param directness Contributions from function `CalculateDirectness()`.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - relative_effect Relative effect for this study.
#' - contribution: Size of contribution for this study.
.FindDirectRegressionContributions <- function(model_output, reference, comparator, directness) {

  treatments <- c()
  covariate_values <- c()
  relative_effects <- c()
  contributions <- c()

  for (treatment in comparator) {
    for (study in row.names(directness$is_direct)) {
      contribution_is_direct <- directness$is_direct[study, treatment]

      if (!contribution_is_direct) {
        next
      }

      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, directness$covariate_value[study])
      relative_effects <- c(relative_effects, directness$relative_effect[study, treatment])
      contributions <- c(contributions, contribution_is_direct)
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

#' Format the credible regions for the regression analysis into a plottable data frame.
#'
#' @param credible_regions List of credible region data frames from function `CalculateCredibleRegions()`.
#' @param comparator Name of comparison treatment for which to find the contributions.
#'
#' @return Data frame containing contribution details. Each row represents a credible interval at a specific covariate value, for a given treatment. Columns are:
#' - Treatment: The treatment for which this credible interval relates.
#' - covariate_value: Value of the covariate for this interval
#' - y_min Relative effect of the lower end of this interval
#' - y_max: Relative effect of the upper end of this interval
.FormatRegressionCredibleRegion <- function(credible_regions, comparator) {

  treatments <- c()
  covariate_values <- c()
  y_mins <- c()
  y_maxs <- c()

  for (treatment_name in comparator) {
    treatment_covariate_values <- credible_regions[[treatment_name]]$cov_value
    treatment_y_mins <- credible_regions[[treatment_name]]$lower
    treatment_y_maxs <- credible_regions[[treatment_name]]$upper

    treatments <- c(treatments, rep(treatment_name, length(treatment_covariate_values)))
    covariate_values <- c(covariate_values, treatment_covariate_values)
    y_mins <- c(y_mins, treatment_y_mins)
    y_maxs <- c(y_maxs, treatment_y_maxs)
  }

  credible_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    y_min = y_mins,
    y_max = y_maxs
  )

  # Return an empty data frame with correct column names if all of the rows are NA
  if (all(is.na(credible_df$covariate_value))) {
    return(data.frame(matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("Treatment", "covariate_value", "y_min", "y_max")))))
  }

  return(credible_df)
}

#' Setup the main components of the plot panel.
#'
#' @param plot The ggplot2 object
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param include_credible TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param credible_opacity The opacity of the credible regions. Can be any value between 0 and 1, inclusive. Defaults to 1.
#'
#' @return Created ggplot2 object.
SetupRegressionPlotColours <- function(plot, comparators, include_ghosts, include_credible, credible_opacity = 1) {
  # Ensure that enough colours are always provided, by cycling the given colours
  base_colours <- c("#bb0000", "#bba000", "#00bb00", "#00bbbb", "#0000bb", "#bb00bb",
                    "#ff5555", "#ffa000", "#44ff44", "#55ffff", "#7744ff", "#ff00ff")
  colours <- rep(base_colours, ceiling(length(comparators) / length(colours)))[1:length(comparators)]

  # Set the colours
  if (include_ghosts) {
    colours <- c("#eeeeee", colours)
  }

  plot <- plot +
    scale_colour_manual(values = colours)

  # Only include fills if credible regions included
  if (include_credible) {
    opacity_hex = format(
      as.hexmode(as.integer(credible_opacity * 255)),
      width = 2
    )
    fills <- paste0(base_colours, opacity_hex)
    fills <- rep(fills, ceiling(length(comparators) / length(fills)))[1:length(comparators)]

    plot <- plot +
      scale_fill_manual(values = fills, guide = "none")
  }

  return(plot)
}

#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param directness Contributions from function `CalculateDirectness()`.
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
    directness,
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
    plot <- .PlotIndirectCovariateCircles(plot, model_output, treatment_df, reference, ghosts, directness, covariate_symbol, covariate_symbol_size, ghosted = TRUE)
  }

  if (length(comparators) > 0) {
    plot <- .PlotIndirectCovariateCircles(plot, model_output, treatment_df, reference, comparators, directness, covariate_symbol, covariate_symbol_size)
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
#' @param directness Contributions from function `CalculateDirectness()`.
#' @param covariate_symbol The selected symbol for displaying covariates. Defaults to "circle open".
#' @param covariate_symbol_size Size of the covariate symbols. Defaults to 10.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotIndirectCovariateCircles <- function(plot, model_output, treatment_df, reference, comparators, directness, covariate_symbol = "circle open", covariate_symbol_size = 10, ghosted = FALSE) {
  contributions = .FindIndirectRegressionCovariates(model_output, reference, comparators, directness)

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
        x = .data$covariate_value,
        y = .data$y_val,
        color = .data$Treatment,
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
#' @param directness Contributions from function `CalculateDirectness()`.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - contribution: Size of contribution for this study.
.FindIndirectRegressionCovariates <- function(model_output, reference, comparator, directness) {
  treatments <- c()
  covariate_values <- c()
  contributions <- c()

  for (treatment in comparator) {
    for (study in row.names(directness$is_indirect)) {
      contribution_is_indirect <- directness$is_indirect[study, treatment]

      if (!contribution_is_indirect) {
        next
      }

      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, directness$covariate_value[study])
      contributions <- c(contributions, contribution_is_indirect)
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
