#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model GEMTC model result object.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param covariate_value The value of the covariate to plot as a vertical line. NULL if not to be plotted.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#'
#' @return Created ggplot2 object.
CreateIndirectContributionPlot <- function(
    model,
    treatment_df,
    reference,
    comparators,
    covariate_value,
    contribution_type,
    include_ghosts = FALSE,
    contribution_multiplier = 1.0) {
  
  comparators <- sort(comparators)
  
  all_comparators <- .FindAllComparators(model, reference)
  
  # Set up basic plot
  plot <- .SetupIndirectContributionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators)
  )
  
  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    plot <- .PlotIndirectContributionCircles(plot, model, treatment_df, reference, ghosts, contribution_type, contribution_multiplier, ghosted = TRUE)
  }
  
  if (length(comparators) > 0) {
    plot <- .PlotIndirectContributionCircles(plot, model, treatment_df, reference, comparators, contribution_type, contribution_multiplier)
  }
  
  if (!is.null(covariate_value)) {
    plot <- plot +
      geom_vline(
        xintercept = covariate_value,
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
  
  return(plot)
}

#' Plot the contribution circles on the plot.
#'
#' @param plot object to which to add elements.
#' @param model GEMTC model result object.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotIndirectContributionCircles <- function(plot, model, treatment_df, reference, comparators, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindIndirectRegressionContributions(model, reference, comparators, contribution_type)
  
  contributions$Treatment <- sapply(contributions$Treatment, function(treatment) { treatment_df$RawLabel[treatment_df$Label == treatment] })
  
  if (ghosted) {
    contributions$Treatment <- rep("Other", length(contributions$Treatment))
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
.FindIndirectRegressionContributions <- function(model, reference, comparator, contribution_type) {
  
  treatments <- c()
  covariate_values <- c()
  contributions <- c()
  
  treatments <- c(treatments, rep("the_Butcher", 3))
  covariate_values <- c(covariate_values, c(3.4, 7, 1))
  contributions <- c(contributions, c(2, 3, 1))
  
  treatments <- c(treatments, rep("the_Dung_named", 2))
  covariate_values <- c(covariate_values, c(1.8, 8))
  contributions <- c(contributions, c(2, 5))
  
  treatments <- c(treatments, rep("the_Great", 3))
  covariate_values <- c(covariate_values, c(1.5, 2.5, 3.5))
  contributions <- c(contributions, c(2, 3, 1))
  
  # the_Little is the reference treatment
  
  treatments <- c(treatments, rep("the_Slit_nosed", 3))
  covariate_values <- c(covariate_values, c(2, 4, 6))
  contributions <- c(contributions, c(2, 3, 5))
  
  treatments <- c(treatments, rep("the_Younger", 3))
  covariate_values <- c(covariate_values, c(5, 7.7, 4.9))
  contributions <- c(contributions, c(6, 6, 2.6))
  
  contribution_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    contribution = contributions
  )
  
  return(contribution_df[contribution_df$Treatment %in% comparator, ])
}

#' Example for the meta-regression main plot.
#'
#' @return Created ggplot2 object.
.MetaRegressionIndirectContributionPlotExample <- function() {
  data <- read.csv("tests/testthat/Cont_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  model <- RunCovariateModel(data, wrangled_treatment_list, "Continuous", 'MD', "covar.age", "age", 'random', 'unrelated', "the_Little")
  
  plot <- CreateIndirectContributionPlot(
    model = model,
    treatment_df = wrangled_treatment_list,
    reference = "the_Little",
    comparators = c("the_Butcher", "the_Dung_named"),
    covariate_value = 4.2,
    contribution_type = "percentage",
    include_ghosts = TRUE,
    contribution_multiplier = 3.5
  )
  
  return(plot)
}
