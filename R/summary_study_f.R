#' Create a forest plot of pairwise comparisons, grouped by treatment pairs.
#'
#' @param connected_data dataframe. Uploaded data created by `setup_load()`
#' @param freq list. Frequentist analysis produced by `setup_configure()` or `setup_exclude()`
#' @param outcome_measure character. One of "MD", "SMD", "OR", "RR", or "RD".
#' @param plot_area_width numeric. The width of the plot area containing the
#' treatment effects in inches. Defaults to `6`.
#' @param colourblind logical. Whether to use a colourblind-friendly palette. Defaults to `FALSE`
#' @param x_min numeric. Minimum value for the x-axis. Defaults to `NULL`
#' @param x_max numeric. Maximum value for the x-axis. Defaults to `NULL`
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @export
summary_study <- function(connected_data, freq, outcome_measure, plot_area_width = 6, colourblind = FALSE, x_min = NULL, x_max = NULL) {

  rob_data_frame <- unique(connected_data[, c("Study", FindRobNames(connected_data))])
  if (!is.data.frame(rob_data_frame)) {
    pairwise <- freq$d1
  } else {
    pairwise <- as.data.frame(merge(freq$d1, rob_data_frame, by = "Study"))
  }

  if (is.null(x_min) || is.nulll(x_max)){
    min_max <- .FindMinMax(pairwise)
    x_min <- min_max$x_min
    x_max <- min_max$x_max
  }

  pairwise_treatments <- PairwiseTreatments(
    pairwise = pairwise,
    treatment_order = freq$lstx
  )

  if (colourblind) {
    palette  <- c("#fed98e", "#fe9929", "#d95f0e")
  } else {
    palette <- c("chartreuse3", "darkgoldenrod1", "red")
  }

  rob_variables <- FindRobNames(pairwise)
  rob_names <- gsub("rob.", "", FindRobNames(connected_data))
  n_rob_variables <- length(rob_variables)

  x_label <- switch(
    outcome_measure,
    "MD" = "Mean difference",
    "SMD" = "Standardised MD",
    "OR" = "Odds ratio",
    "RR" = "Risk ratio",
    "RD" = "Risk difference"
  )

  x_ticks <- .FindXTicks(lower = x_min, upper = x_max)
  increment <- x_ticks[2] - x_ticks[1]

  inches_to_lines <- par("cin")[2]

  # process pairwise data needed for layout
  pairwise$point_estimate <- pairwise$TE
  pairwise$ci_lower <- pairwise$TE - 1.96 * pairwise$seTE
  pairwise$ci_upper <- pairwise$TE + 1.96 * pairwise$seTE
  if (is.element(outcome_measure, c("OR", "RR"))) {
    pairwise$point_estimate <- exp(pairwise$point_estimate)
    pairwise$ci_lower <- exp(pairwise$ci_lower)
    pairwise$ci_upper <- exp(pairwise$ci_upper)
  }
  pairwise$point_estimate <- pairwise$point_estimate |> sprintf(fmt = "%-.2f")
  pairwise$ci_lower <- pairwise$ci_lower |> sprintf(fmt = "%-.2f")
  pairwise$ci_upper <- pairwise$ci_upper |> sprintf(fmt = "%-.2f")
  pairwise$outcome <- paste0(pairwise$point_estimate, " (", pairwise$ci_lower, ", ", pairwise$ci_upper,  ")")

  # find the longest outcome label
  outcome_width_inches <- max(strwidth(pairwise$outcome, units = "in"))
  outcome_width <- outcome_width_inches / inches_to_lines

  # find the longest label in lines
  longest_treatment_label_inches <- max(strwidth(paste(pairwise$treat1, " vs. ", pairwise$treat2), units = "in"))
  longest_study_label_inches <- max(strwidth(pairwise$studlab, units = "in")) + inches_to_lines # + 1 for offsetting
  label_width_inches <- max(longest_treatment_label_inches, longest_study_label_inches)
  label_width <- label_width_inches / inches_to_lines

  # set plot dimensions and margins
  top_line <- max(pairwise_treatments$y_position_last)

  bottom_margin <- 4 + ifelse(n_rob_variables > 3, n_rob_variables, 3)
  top_margin <- 4
  left_margin <- 4 + (outcome_width_inches + label_width_inches + (2 * inches_to_lines)) / inches_to_lines
  right_margin <- 4 + n_rob_variables

  plot_height <- (bottom_margin + top_margin + top_line) * inches_to_lines

  plot_width <- outcome_width_inches +
                label_width_inches +
                (2 * inches_to_lines) +
                plot_area_width +
                (n_rob_variables * inches_to_lines)

  svg <- svglite::xmlSVG({
    par(family = "Arimo")
    par(mar = c(bottom_margin, left_margin, top_margin, right_margin))

    plot(
      x = NA,
      y = NA,
      xlim = c(x_min, x_max),
      ylim = c(0, max(pairwise_treatments$y_position_last) + 2),
      yaxt = "n",
      xaxt = "n",
      ylab = "",
      xlab = x_label,
      frame.plot = F, # remove border
      yaxs = "i" # remove internal padding
    )

    title("Individual study results (with selected studies excluded)\n grouped by treatment comparison", line = 2)

    # define the ticks and labels for the x-axis
    if (is.element(outcome_measure, c("OR", "RR"))) {
      x_labels <- signif(exp(x_ticks), digits = 2)
    } else if (is.element(outcome_measure, c("MD", "SMD", "RD"))) {
      x_labels <- x_ticks
    }

    # final coordinates
    label_x_position <- label_width + 1 + outcome_width + 1
    outcome_x_position <- outcome_width + 1
    legend_x_position <- label_x_position

    # add the x-axis
    axis(
      side = 1,
      at = x_ticks,
      labels = x_labels
    )

    # add the line of no effect
    lines(
      x = rep(0, times = 2),
      y = c(0, top_line),
      lty = 2
    )

    # add header of the within-study comparison-level outcomes
    mtext(
      text = c(x_label, "(95% CI)"),
      side = 2,
      at = c(top_line + 2.2, top_line + 1),
      line = 1 + (outcome_width * 0.5), # centered
      adj = 0.5,
      las = 1,
      font = 2
    )

    if (n_rob_variables > 0) {

      # ROB labels above markers
      rob_letters <- LETTERS[1:12][1:n_rob_variables]
      rob_label_y <- max(pairwise_treatments$y_position_last) + 1
      rob_x_positions <- 2:(n_rob_variables + 1)

      rob_or_ind_indices <- which(is.element(rob_variables, c("rob", "indirectness")))
      n_rob_or_ind <- length(rob_or_ind_indices)
      # If there are individual rob variables and at least one of rob or indirectness, separate the overall circles
      if (n_rob_or_ind > 0 && length(rob_variables) > n_rob_or_ind) {
        rob_x_positions[rob_or_ind_indices] <- rob_x_positions[rob_or_ind_indices] + 0.75
      }

      mtext(
        rob_letters,
        side = 4,
        line = rob_x_positions,
        at = rob_label_y,
        las = 1,
        adj = 0.5,
        font = 2
      )

      # ROB legend
      rob_legend <- paste(rob_letters, rob_names, sep = ": ")
      rob_legend_y <- -(2:(n_rob_variables + 1))
      legend_width <- max(strwidth(rob_legend, units = "inches")) / inches_to_lines

      # legend for domains
      mtext(
        rob_legend,
        side = 2,
        line = legend_x_position,
        las = 1,
        at = rob_legend_y,
        adj = 0
      )

      # legend for colours
      rob_levels <- c("Low risk of bias", "Some concerns", "High risk of bias")
      mtext(
        rep("•", 3),
        side = 2,
        line = legend_x_position - legend_width - 1,
        las = 1,
        at = -2:-4,
        adj = 0,
        padj = 0.5,
        cex = 3,
        col = palette
      )

      mtext(
        rob_levels,
        side = 2,
        line = legend_x_position - legend_width - 2,
        las = 1,
        at = -2:-4,
        adj = 0,
        padj = 0.5
      )

    }


    for (row in 1:length(pairwise_treatments[[1]])) {
      .AddTreatmentEffectBlock(
        pairwise = pairwise,
        label_x_position = label_x_position,
        outcome_x_position = outcome_x_position,
        rob_x_positions = rob_x_positions,
        y_header_position = pairwise_treatments$y_position_last[row],
        y_positions = (pairwise_treatments$y_position_first[row] + 1):(pairwise_treatments$y_position_last[row] - 1),
        treatment1 = pairwise_treatments$treat1[row],
        treatment2 = pairwise_treatments$treat2[row],
        rob_variables = rob_variables,
        palette = palette
      )
    }
  },
  width = plot_width,
  height = plot_height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)
}



#' Finds suitable tick positions for the x-axis.
#'
#' @param lower x-axis minimum.
#' @param upper x-axis maximum.
#' @return Vector of x-axis ticks.
.FindXTicks <- function(lower, upper) {
  #If the interval is entirely above 0 then replace the lower limit with 0
  lower <- min(0, lower)
  #If the interval is entirely below 0 then replace the upper limit with 0
  upper <- max(0, upper)
  difference <- upper - lower
  # increment <- 10 ^ floor(log10(difference))
  increment <- round(difference / 4, digits = 1)
  #Round down according to the increment
  lower <- increment * floor(lower / increment)
  #Round up according to the increment
  upper <- increment * ceiling(upper / increment)
  return(seq(from = lower, to = upper, by = increment))
}



#' Add the treatment effect line and confidence interval for a single comparison in a single study.
#'
#' @param pairwise Output from meta::pairwise().
#' @param y_position y co-ordinate for the study label and confidence interval.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param ci_limit_height Height of the vertical lines at the end of the confidence intervals. Defaults to 0.3.
.AddTreatmentEffect <- function(pairwise, y_position, studlab, treatment1, treatment2, ci_limit_height = 0.3) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  # point estimate
  points(
    x = pairwise$TE[row],
    y = y_position,
    pch = 18
  )
  # horizontal line
  lines(
    x = c(pairwise$TE[row] - 1.96 * pairwise$seTE[row], pairwise$TE[row] + 1.96 * pairwise$seTE[row]),
    y = rep(y_position, times = 2)
  )
  # vertical lines
  lines(
    x = rep(pairwise$TE[row] - 1.96 * pairwise$seTE[row], times = 2),
    y = c(y_position - ci_limit_height, y_position + ci_limit_height)
  )
  lines(
    x = rep(pairwise$TE[row] + 1.96 * pairwise$seTE[row], times = 2),
    y = c(y_position - ci_limit_height, y_position + ci_limit_height)
  )
}



#' Add the effects size and confidence interval text for a single comparison in a single study.
#'
#' @param pairwise Output from meta::pairwise().
#' @param x_position x co-ordinate for the start of the text.
#' @param y_position y co-ordinate for the effect size and confidence interval.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
.AddOutcomeText <- function(pairwise, x_position, y_position, studlab, treatment1, treatment2) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  mtext(
    text = paste0(pairwise$point_estimate[row], " (", pairwise$ci_lower[row], ", ", pairwise$ci_upper[row],  ")"),
    side = 2,
    line = x_position,
    at = y_position,
    las = 1,
    adj = 0
  )
}



#' Add risk of bias circles for a single comparison in a single study.
#'
#' @param pairwise Output from meta::pairwise().
#' @param x_positions x co-ordinate for the circles.
#' @param y_position y co-ordinate for the circles.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
#' @param palette Vector of colours to use
.AddRiskOfBias <- function(pairwise, x_positions, y_position, studlab, treatment1, treatment2, rob_variables, palette) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  colours <- palette

  mtext(
    "•",
    side = 4,
    at = y_position,
    line = x_positions,
    las = 1,
    adj = 0.5,
    col = colours[sapply(pairwise[row, rob_variables], function(x){x[[1]]})],
    cex = 3
  )
}


#' Add the treatment effect lines, confidence intervals, studies, and comparison header, for a single comparison.
#'
#' @param pairwise Output from meta::pairwise().
#' @param label_x_position  x co-ordinate for the start of the labels.
#' @param outcome_x_position x co-ordinate for the start of the text.
#' @param rob_x_positions x co-ordinates for the circles.
#' @param y_header_position y co-ordinate for the header.
#' @param y_positions Vector of y co-ordinates for the study labels and confidence intervals.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
#' @param palette Vector of colours to use
.AddTreatmentEffectBlock <- function(pairwise, label_x_position, outcome_x_position, rob_x_positions, y_header_position, y_positions, treatment1, treatment2, rob_variables, palette) {

  # header
  mtext(
    text = paste0(treatment1, " vs. ", treatment2),
    side = 2,
    line = label_x_position,
    at = y_header_position + 0.2,
    las = 1,
    adj = 0,
    font = 2
  )

  # select the rows comparing to the two treatments
  pairwise_subset <- pairwise[
    pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2
    ,
  ]

  # study labels
  mtext(
    text = pairwise_subset$studlab,
    side = 2,
    line = label_x_position - 1,
    at = y_positions,
    las = 1,
    adj = 0
  )

  include_rob <- length(rob_variables) > 0

  for (row in 1:length(pairwise_subset$studlab)) {
    .AddTreatmentEffect(
      pairwise = pairwise,
      y_position = y_positions[row],
      studlab = pairwise_subset$studlab[row],
      treatment1 = pairwise_subset$treat1[row],
      treatment2 = pairwise_subset$treat2[row]
    )
    .AddOutcomeText(
      pairwise = pairwise,
      x_position = outcome_x_position,
      y_position = y_positions[row],
      studlab = pairwise_subset$studlab[row],
      treatment1 = pairwise_subset$treat1[row],
      treatment2 = pairwise_subset$treat2[row]
    )
    if (include_rob) {
      .AddRiskOfBias(
        pairwise = pairwise,
        x_positions = rob_x_positions,
        y_position = y_positions[row],
        studlab = pairwise_subset$studlab[row],
        treatment1 = pairwise_subset$treat1[row],
        treatment2 = pairwise_subset$treat2[row],
        rob_variables = rob_variables,
        palette = palette
      )
    }
  }
}



#' Creates a data frame of values used to set y co-ordinates.
#'
#' @param pairwise Output from meta::pairwise().
#' @param treatment_order Vector of the treatments in the order they should appear in the forest plot.
#' @return Data frame with columns:
#'  - 'treat1': First treatment in the comparison.
#'  - 'treat2': Second treatment in the comparison.
#'  - 'y_position_first': y co-ordinate of the lowest point on the graph corresponding to this comparison.
#'  - 'y_position_last': y co-ordinate of the highest point on the graph corresponding to this comparison.
PairwiseTreatments <- function(pairwise, treatment_order) {
  #The number of studies comparing each pair of treatments
  pairwise_treatments <- as.data.frame(table(pairwise[, c("treat1", "treat2")]))
  #Delete any rows where the frequency is 0
  pairwise_treatments <- pairwise_treatments[pairwise_treatments$Freq != 0, ]
  #Sort the data frame in reverse order of 'treatment_order' (which results in the forest plot the right way round because the y-axis moves upwards and the forest plot is read downwards)
  pairwise_treatments$order1 <- match(x = pairwise_treatments$treat1, table = treatment_order)
  pairwise_treatments$order2 <- match(x = pairwise_treatments$treat2, table = treatment_order)
  pairwise_treatments <- pairwise_treatments[
    order(rev(pairwise_treatments$order1), rev(pairwise_treatments$order2))
    ,
  ]
  #The number of rows required for each comparison, including the header and one blank row
  pairwise_treatments$n_forest_rows <- pairwise_treatments$Freq + 2
  #The y co-ordinate of the last row (which is the blank one)
  pairwise_treatments$y_position_last <- cumsum(pairwise_treatments$n_forest_rows)
  #The y co-ordinate of the first row (which is the header)
  pairwise_treatments$y_position_first <- pairwise_treatments$y_position_last - pairwise_treatments$n_forest_rows + 1
  #Keep only columns of interest
  pairwise_treatments <- pairwise_treatments[
    ,
    c("treat1", "treat2", "y_position_first", "y_position_last")
  ]
  return(pairwise_treatments)
}

#' Find the default minimum and maximum values for the x-axis and calculate
#' a sensible step to use in the numeric input
#' @param pairwise Results of pairwise analysis
#' @return List containing:
#'  \item{x_min}{numeric. Minimum value for the x-axis}
#'  \item{x_max}{numeric. Maximum value for the x-axis}
#'  \item{step}{numeric. Step value to use in numericInput}
.FindMinMax <- function(pairwise){
  x_min <- min(pairwise$TE - 1.96 * pairwise$seTE)
  x_max <- max(pairwise$TE + 1.96 * pairwise$seTE)
  x_ticks <- .FindXTicks(x_min, x_max)
  increment <- x_ticks[2] - x_ticks[1]
  step <- 10 ^ floor(log10(increment / 10))

  x_min <- round(x_min, max(0, -log10(step)))
  x_max <- round(x_max, max(0, -log10(step)))

  list(x_min = x_min,
       x_max = x_max,
       step = step)
}


#' Find the default minimum and maximum values for the x-axis and calculate
#' a sensible step to use in the numeric input
#'
#' @param connected_data dataframe. Uploaded data created by `setup_load()`
#' @param freq list. Frequentist analysis produced by `setup_configure()` or `setup_exclude()`
#' @return List containing:
#'  \item{x_min}{numeric. Minimum value for the x-axis}
#'  \item{x_max}{numeric. Maximum value for the x-axis}
#'  \item{step}{numeric. Step value to use in numericInput}
#'
#' @export
summary_study_min_max <- function(connected_data, freq){
  rob_data_frame <- unique(connected_data[, c("Study", FindRobNames(connected_data))])
  if (!is.data.frame(rob_data_frame)) {
    pairwise <- freq$d1
  } else {
    pairwise <- as.data.frame(merge(freq$d1, rob_data_frame, by = "Study"))
  }
  .FindMinMax(pairwise)
}

