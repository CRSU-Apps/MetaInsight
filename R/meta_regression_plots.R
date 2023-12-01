CreateRegressionPlot <- function(model, reference, comparators, include_ghosts = FALSE, include_extrapolation = FALSE, contribution_multiplier = 1.0, include_confidence = FALSE) {
  
  # Set up basic plot
  plot <- .SetupMainPlot(reference, comparators, include_ghosts)
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    all_comparators <- .GetComparators(model, reference)
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    
    plot <- .PlotContributionCircles(plot, model, reference, ghosts, contribution_multiplier, ghosted = TRUE)
    plot <- .PlotRegressionLines(plot, model, reference, ghosts, include_extrapolation, ghosted = TRUE)
  }
  
  if (include_confidence) {
    plot <- .PlotConfidenceRegions(plot, model, reference, comparators)
  }
  plot <- .PlotContributionCircles(plot, model, reference, comparators, contribution_multiplier)
  plot <- .PlotRegressionLines(plot, model, reference, comparators, include_extrapolation)
  
  return(plot)
}

.SetupMainPlot <- function(reference, comparators, include_ghosts) {
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
  colours <- c("red", "orange", "yellow", "green", "cyan", "blue", "magenta")
  colours <- rep(colours, ceiling(length(comparators) / length(colours)))[1:length(comparators)]
  fills <- c("#ff000030", "#ffaa0030", "#ffff0030", "#00ff0030", "#00ffff30", "#0000ff30", "#ff00ff30")
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

.PlotConfidenceRegions <- function(plot, model, reference, comparators) {
  confidence <- .GetRegressionConfidenceRegion(model, reference, comparators)
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
  
.PlotContributionCircles <- function(plot, model, reference, comparators, contribution_multiplier, ghosted = FALSE) {
  
  # Cirtcles for coontributions
  contributions = .GetRegressionContributions(model, reference, comparators)
  
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

.PlotRegressionLines <- function(plot, model, reference, comparators, extrapolate, ghosted = FALSE) {
  # Create data frame
  lines = data.frame(
    Treatment = comparators,
    intersect = .GetRegressionIntersect(model, reference, comparators),
    slope = .GetRegressionGradient(model, reference, comparators),
    start_x = .GetRegressionStartX(model, reference, comparators),
    end_x = .GetRegressionEndX(model, reference, comparators)
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
        y = .GetRegressionY(start_x, intersect, slope),
        xend = end_x,
        yend = .GetRegressionY(end_x, intersect, slope),
        color = Treatment
      ),
      linewidth = 1.2,
      show.legend = !ghosted
    )
  
  return(plot)
}
















.GetComparators <- function(model, reference) {
  treatments <- model$model$network$treatments$description
  return(treatments[treatments != reference])
}

.GetRegressionIntersect <- function(model, reference, comparator) {
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

.GetRegressionGradient <- function(model, reference, comparator) {
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

.GetRegressionStartX <- function(model, reference, comparator) {
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

.GetRegressionEndX <- function(model, reference, comparator) {
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

.GetRegressionY <- function(x, intersept, slope) {
  return(intersept + slope * x)
}

.GetRegressionContributions <- function(model, reference, comparator) {
  
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

.GetRegressionConfidenceRegion <- function(model, reference, comparator, divisions = 10) {
  
  treatments <- c()
  covariate_values <- c()
  y_mins <- c()
  y_maxs <- c()
  
  for (treatment in c("the_Butcher", "the_Dung_named", "the_Great", "the_Slit_nosed", "the_Younger")) {
    start_x <- .GetRegressionStartX(model, reference, treatment)
    end_x <- .GetRegressionEndX(model, reference, treatment)
    intersect <- .GetRegressionIntersect(model, reference, treatment)
    gradient <- .GetRegressionGradient(model, reference, treatment)
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














tasty <- function() {
  data <- read.csv("tests/testthat/Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  model <- RunCovariateModel(data, wrangled_treatment_list, "Binary", 'OR', "covar.age", "age", 'random', 'unrelated', "the_Little")
  
  # plot <- CreateRegressionPlot(model, c("the_Great", "the_Younger"), "the_Little", include_ghosts = TRUE)
  plot <- CreateRegressionPlot(
    model = model,
    reference = "the_Little",
    comparators = c("the_Butcher", "the_Dung_named"),
    include_ghosts = TRUE,
    contribution_multiplier = 5.0,
    include_extrapolation = TRUE,
    include_confidence = TRUE
  )
  
  return(plot)
}



