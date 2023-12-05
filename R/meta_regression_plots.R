#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contribution TRUE if study contribution should be displayed as circles. Defaults to TRUE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#'
#' @return Created ggplot2 object.
CreateRegressionPlot <- function(
    model,
    reference,
    comparators,
    contribution_type,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0) {
  
  comparators <- sort(comparators)
  
  all_comparators <- .FindAllComparators(model, reference)
  
  # Set up basic plot
  plot <- .SetupMainPlot(reference, comparators, include_ghosts && length(comparators) < length(all_comparators), confidence_opacity)
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    
    if (include_contributions) {
      plot <- .PlotContributionCircles(plot, model, reference, ghosts, contribution_type, contribution_multiplier, ghosted = TRUE)
    }
    plot <- .PlotRegressionLines(plot, model, reference, ghosts, include_extrapolation, ghosted = TRUE)
  }
  
  if (length(comparators) > 0) {
    if (include_confidence) {
      plot <- .PlotConfidenceRegions(plot, model, reference, comparators)
    }
    if (include_contributions) {
      plot <- .PlotContributionCircles(plot, model, reference, comparators, contribution_type, contribution_multiplier)
    }
    plot <- .PlotRegressionLines(plot, model, reference, comparators, include_extrapolation)
  }
  
  return(plot)
}

#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all otherc omparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#'
#' @return Created ggplot2 object.
.SetupMainPlot <- function(reference, comparators, include_ghosts, confidence_opacity) {
  # Set up basic plot
  plot <- ggplot() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      panel.border = element_rect(colour = "#777777", fill = NA, linewidth = 2),
      
      legend.position = c(.99, .02),
      legend.justification = c("right", "bottom"),
      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(colour = "black", linewidth = 0.5, fill = "#ffffffaa"),
      legend.box.just = c("right", "bottom")
    ) +
    xlab("Covariate Value") +
    ylab(glue::glue("Relative Effect vs {reference}"))
  
  # Ensure that enough colours are always provided, by cycling the given colours
  base_colours <- c("#bb0000", "#bba000", "#00bb00", "#00bbbb", "#0000bb", "#bb00bb",
                    "#ff5555", "#ffa000", "#44ff44", "#55ffff", "#7744ff", "#ff00ff")
  colours <- rep(base_colours, ceiling(length(comparators) / length(colours)))[1:length(comparators)]
  
  opacity_hex = format(
    as.hexmode(as.integer(confidence_opacity * 255)),
    width = 2
  )
  fills <- paste0(base_colours, opacity_hex)
  fills <- rep(fills, ceiling(length(comparators) / length(fills)))[1:length(comparators)]
  
  # Set the colours
  if (include_ghosts) {
    colours <- c("#eeeeee", colours)
  }
  
  plot <- plot +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = fills, guide = "none")
  
  return(plot)
}

#' Plot the confidence regions on the plot.
#'
#' @param plot object to which to add elements.
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#'
#' @return The modified ggplot2 object.
.PlotConfidenceRegions <- function(plot, model, reference, comparators) {
  confidence <- .FindRegressionConfidenceRegion(model, reference, comparators)
  plot <- plot +
    geom_ribbon(
      data = confidence,
      mapping = aes(
        x = covariate_value,
        ymin = y_min,
        ymax = y_max,
        fill = Treatment
      ),
      show.legend = FALSE,
    )
  
  return(plot)
}

#' Plot the contribution circles on the plot.
#'
#' @param plot object to which to add elements.
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotContributionCircles <- function(plot, model, reference, comparators, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindRegressionContributions(model, reference, comparators, contribution_type)
  
  if (ghosted) {
    contributions$Treatment <- rep("Other", length(contributions$Treatment))
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
#' @param plot object to which to add elements.
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param extrapolate TRUE if regression lines should be extrapolated beyond the range of the data. These will be plotted as dashed lines.
#' Defaults to FALSE.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotRegressionLines <- function(plot, model, reference, comparators, extrapolate, ghosted = FALSE) {
  # Create data frame
  lines = data.frame(
    Treatment = comparators,
    intersect = .FindRegressionIntersect(model, reference, comparators),
    slope = .FindRegressionGradient(model, reference, comparators),
    start_x = .FindRegressionStartX(model, reference, comparators),
    end_x = .FindRegressionEndX(model, reference, comparators)
  )
  
  if (ghosted) {
    lines$Treatment <- rep("Other", length(lines$Treatment))
  }
  
  if (extrapolate) {
    # Dashed lines outside of data ranges
    plot <- plot +
      geom_abline(
        data = lines,
        mapping = aes(
          intercept = intersect,
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
        y = intersect + slope * start_x,
        xend = end_x,
        yend = intersect + slope * end_x,
        color = Treatment
      ),
      linewidth = 1.2,
      show.legend = !ghosted
    )
  
  return(plot)
}

#' Find the names of all possible comparators in the model.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#'
#' @return Vector containing all treatments other than the reference treatment.
.FindAllComparators <- function(model, reference) {
  treatments <- model$model$network$treatments$description
  return(treatments[treatments != reference])
}

#' Find the relative effect at a covariate value of zero.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find intersect.
#'
#' @return The relative effect axis intersect.
.FindRegressionIntersect <- function(model, reference, comparator) {
  intersects <- data.frame(
    treatment = c(
      "the_Butcher",
      "the_Dung_named",
      "the_Great",
      "the_Little",
      "the_Slit_nosed",
      "the_Younger"
    ),
    value = c(1, 2, 4, NA, 7, 11)
  )
  return(intersects$value[intersects$treatment %in% comparator])
}

#' Find the relative effect vs covariate value gradient.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find gradient.
#'
#' @return The relative effect vs covariate value gradient.
.FindRegressionGradient <- function(model, reference, comparator) {
  gradients <- data.frame(
    treatment = c(
      "the_Butcher",
      "the_Dung_named",
      "the_Great",
      "the_Little",
      "the_Slit_nosed",
      "the_Younger"
    ),
    value = c(1.0, 0.4, -0.2, NA, -0.8, -1.4)
  )
  return(gradients$value[gradients$treatment %in% comparator])
}

#' Find the lowest covariate value given by a study comparing the reference and comparator treatments.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find lowest covariate value.
#'
#' @return The lowest covariate value of a relevant study.
.FindRegressionStartX <- function(model, reference, comparator) {
  intersects <- data.frame(
    treatment = c(
      "the_Butcher",
      "the_Dung_named",
      "the_Great",
      "the_Little",
      "the_Slit_nosed",
      "the_Younger"
    ),
    value = c(0, 1, 2, NA, 3, 4)
  )
  return(intersects$value[intersects$treatment %in% comparator])
}

#' Find the highest covariate value given by a study comparing the reference and comparator treatments.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find highest covariate value.
#'
#' @return The highest covariate value of a relevant study.
.FindRegressionEndX <- function(model, reference, comparator) {
  intersects <- data.frame(
    treatment = c(
      "the_Butcher",
      "the_Dung_named",
      "the_Great",
      "the_Little",
      "the_Slit_nosed",
      "the_Younger"
    ),
    value = c(6, 7, 8, NA, 9, 10)
  )
  return(intersects$value[intersects$treatment %in% comparator])
}

#' Find the contributions to the regression analysis.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param contribution_type Type of contribution to find.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - relative_effect Relative effect for this study.
#' - contribution: Size of contribution for this study.
.FindRegressionContributions <- function(model, reference, comparator, contribution_type) {
  
  treatments <- c()
  covariate_values <- c()
  relative_effects <- c()
  contributions <- c()
  
  treatments <- c(treatments, rep("the_Butcher", 3))
  covariate_values <- c(covariate_values, c(0, 7, 1))
  relative_effects <- c(relative_effects, c(0.5, 8, 2))
  contributions <- c(contributions, c(2, 3, 1))
  
  treatments <- c(treatments, rep("the_Dung_named", 2))
  covariate_values <- c(covariate_values, c(1, 8))
  relative_effects <- c(relative_effects, c(2, 4.8))
  contributions <- c(contributions, c(2, 5))
  
  treatments <- c(treatments, rep("the_Great", 3))
  covariate_values <- c(covariate_values, c(1.5, 2.5, 3.5))
  relative_effects <- c(relative_effects, c(1.5, 2.5, 3.5))
  contributions <- c(contributions, c(2, 3, 1))
  
  # the_Little is the reference treatment
  
  treatments <- c(treatments, rep("the_Slit_nosed", 3))
  covariate_values <- c(covariate_values, c(2, 4, 6))
  relative_effects <- c(relative_effects, c(6, 2, 4))
  contributions <- c(contributions, c(2, 3, 5))
  
  treatments <- c(treatments, rep("the_Younger", 3))
  covariate_values <- c(covariate_values, c(5, 7.7, 9))
  relative_effects <- c(relative_effects, c(9, 3, 6))
  contributions <- c(contributions, c(6, 6, 6))
  
  contribution_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    relative_effect = relative_effects,
    contribution = contributions
  )
  
  return(contribution_df[contribution_df$Treatment %in% comparator, ])
}

#' Find the confidence regions for the regression analysis.
#'
#' @param model GEMTC model result object.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param divisions The number of divisions the confidence region should be drawn in. Defaults to 10.
#'
#' @return Data frame containing contribution details. Each row represents a confidence interval at a specific covariate value, for a given treatment. Columns are:
#' - Treatment: The treatment for which this confidence interval relates.
#' - covariate_value: Value of the covariate for this interval
#' - y_min Relative effect of the lower end of this interval
#' - y_max: Relative effect of the upper end of this interval
.FindRegressionConfidenceRegion <- function(model, reference, comparator, divisions = 10) {
  
  treatments <- c()
  covariate_values <- c()
  y_mins <- c()
  y_maxs <- c()
  
  for (treatment in c("the_Butcher", "the_Dung_named", "the_Great", "the_Slit_nosed", "the_Younger")) {
    start_x <- .FindRegressionStartX(model, reference, treatment)
    end_x <- .FindRegressionEndX(model, reference, treatment)
    intersect <- .FindRegressionIntersect(model, reference, treatment)
    gradient <- .FindRegressionGradient(model, reference, treatment)
    for (covariate_value in seq(from = start_x, to = end_x, by = (end_x - start_x) / divisions)) {
      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, covariate_value)
      y_mins <- c(
        y_mins,
        intersect + (gradient * covariate_value) - (0.04 * ((covariate_value - 5) ^ 2) + 1)
      )
      y_maxs <- c(
        y_maxs,
        intersect + (gradient * covariate_value) + 0.04 * ((covariate_value - 5) ^ 2) + 1
      )
    }
  }
  
  confidence_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    y_min = y_mins,
    y_max = y_maxs
  )
  
  return(confidence_df[confidence_df$Treatment %in% comparator, ])
}

#' Example for the meta-regression main plot.
#'
#' @return Created ggplot2 object.
.MetaRegressionPlotExample <- function() {
  data <- read.csv("tests/testthat/Cont_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  model <- RunCovariateModel(data, wrangled_treatment_list, "Continuous", 'MD', "covar.age", "age", 'random', 'unrelated', "the_Little")
  
  plot <- CreateRegressionPlot(
    model = model,
    reference = "the_Little",
    comparators = c("the_Butcher", "the_Dung_named"),
    contribution_type = "percentage",
    include_ghosts = TRUE,
    include_extrapolation = TRUE,
    include_confidence = TRUE,
    confidence_opacity = 0.2,
    contribution_multiplier = 5.0
  )
  
  return(plot)
}
