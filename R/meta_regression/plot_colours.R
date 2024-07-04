
#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param include_confidence TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 1.
#'
#' @return Created ggplot2 object.
SetupRegressionPlotColours <- function(plot, comparators, include_ghosts, include_confidence, confidence_opacity = 1) {
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
  
  # Only include fills if confidence regions included
  if (include_confidence) {
    opacity_hex = format(
      as.hexmode(as.integer(confidence_opacity * 255)),
      width = 2
    )
    fills <- paste0(base_colours, opacity_hex)
    fills <- rep(fills, ceiling(length(comparators) / length(fills)))[1:length(comparators)]
    
    plot <- plot +
      scale_fill_manual(values = fills, guide = "none")
  }
  
  return(plot)
}