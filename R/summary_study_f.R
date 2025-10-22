#' Create a forest plot of pairwise comparisons, grouped by treatment pairs.
#'
#' @param pairwise Output from meta::pairwise().
#' @param treatment_order Vector of the treatments in the order they should appear in the forest plot.
#' @param outcome_measure One of "MD", "SMD", "OR", "RR", or "RD".
#' @param x_limits Named vector of x-axis limits in the form (lower, upper).
#' @export
#' @return Forest plot.
summary_study <- function(data, freq, outcome_measure, colourblind = FALSE, x_min = NULL, x_max = NULL) {

  rob_data_frame <- unique(data[, c("Study", FindRobNames(data))])
  if (!is.data.frame(rob_data_frame)) {
    pairwise <- freq$d1
  } else {
    pairwise <- as.data.frame(merge(freq$d1, rob_data_frame, by = "Study"))
  }

  pairwise_treatments <- PairwiseTreatments(
    pairwise = pairwise,
    treatment_order = freq$lstx
  )

  if (colourblind){
    palette  <- c("#fed98e", "#fe9929", "#d95f0e")
  } else {
    palette <- c("chartreuse3", "darkgoldenrod1", "red")
  }



  # x_width <- unname(x_limits["upper"] - x_limits["lower"])
  # x_ticks <- .FindXTicks(lower = x_limits["lower"], upper = x_limits["upper"])

  rob_variables <- FindRobNames(pairwise)
  rob_names <- gsub("rob.", "", FindRobNames(data))
  n_rob_variables <- length(rob_variables)

  x_label <- switch(
    outcome_measure,
    "MD" = "Mean difference",
    "SMD" = "Standardised MD",
    "OR" = "Odds ratio",
    "RR" = "Risk ratio",
    "RD" = "Risk difference"
  )

  if (is.element(outcome_measure, c("MD", "SMD"))) {
    outcome_type <- "continuous"
  } else if (is.element(outcome_measure, c("OR", "RR", "RD"))) {
    outcome_type <- "binary"
  }


  # plot height
  # The rows that don't correspond to NA treatment effects
  proper_comparison_rows <- !is.na(freq$d0$TE)
  # The number of comparisons, with NA rows dropped
  n_proper_comparisons <- length(freq$d0$TE[proper_comparison_rows])
  # The number of unique treatments, with NA rows dropped
  n_proper_treatments <- length(unique(c(freq$d0$treat1[proper_comparison_rows],
                                         freq$d0$treat2[proper_comparison_rows])))

  if (is.null(x_min)){
    x_min <- min(pairwise$TE - 1.96 * pairwise$seTE)
  }

  if (is.null(x_max)){
    x_max <- max(pairwise$TE + 1.96 * pairwise$seTE)
  }

  x_ticks <- .FindXTicks(lower = x_min, upper = x_max)

  inches_to_lines <- par("cin")[2]
  top_line <- max(pairwise_treatments$y_position_last)

  bottom_margin <- 4 + ifelse(n_rob_variables > 3, n_rob_variables, 3)
  top_margin <- 4

  plot_height <- (bottom_margin + top_margin + top_line) * inches_to_lines

  # first attempt, but plot should be narrower when no ROB exist
  plot_width <- ifelse(n_rob_variables > 0, 900 / 72, 800 / 72)

  svglite::xmlSVG({
    par(family = "Arimo")
    par(mar = c(bottom_margin, 20, top_margin, 25))

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
    if (outcome_type == "binary") {
      x_labels <- signif(exp(x_ticks), digits = 1)
    } else if (outcome_type == "continuous") {
      x_labels <- x_ticks
    }

    pairwise$point_estimate <- pairwise$TE
    pairwise$ci_lower <- pairwise$TE - 1.96 * pairwise$seTE
    pairwise$ci_upper <- pairwise$TE + 1.96 * pairwise$seTE
    if (outcome_type == "binary") {
      pairwise$point_estimate <- exp(pairwise$point_estimate)
      pairwise$ci_lower <- exp(pairwise$ci_lower)
      pairwise$ci_upper <- exp(pairwise$ci_upper)
    }
    pairwise$point_estimate <- pairwise$point_estimate |> sprintf(fmt = "%-.2f")
    pairwise$ci_lower <- pairwise$ci_lower |> sprintf(fmt = "%-.2f")
    pairwise$ci_upper <- pairwise$ci_upper |> sprintf(fmt = "%-.2f")
    pairwise$outcome <- paste0(pairwise$point_estimate, " (", pairwise$ci_lower, ", ", pairwise$ci_upper,  ")")

    # find the longest label - needs to be after plot() is called
    longest_treatment_label <- max(strwidth(paste(pairwise$treat1, " vs. ", pairwise$treat2), units = "in") / inches_to_lines)
    longest_study_label <- max(strwidth(pairwise$studlab, units = "in") / inches_to_lines) + 1
    longest_label <- max(longest_treatment_label, longest_study_label)

    # user coordinates needed for legend
    longest_treatment_label_user <- max(strwidth(paste(pairwise$treat1, " vs. ", pairwise$treat2)))
    longest_study_label_user <- max(strwidth(pairwise$studlab))
    x_range_offset <- abs(par("usr")[1])
    longest_label_user <- max(longest_treatment_label_user, longest_study_label_user) + x_range_offset

    outcome_width <- max(strwidth(pairwise$outcome, units = "in") / inches_to_lines)

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
      side = 4,
      at = c(top_line + 2.2, top_line + 1),
      line = 1 + (outcome_width * 0.5), # centered
      adj = 0.5,
      las = 1,
      font = 2
    )

    n_rob <- length(rob_variables)
    start_rob <- outcome_width + 2
    if (n_rob > 0){

      # add ROB labels
      rob_letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")[1:n_rob]
      rob_label_y <- max(pairwise_treatments$y_position_last) + 1
      rob_label_x <- start_rob:(start_rob + n_rob - 1)

      mtext(rob_letters, side = 4, line = rob_label_x, at = rob_label_y, las = 1, adj = 0.5, font = 2)

      rob_legend <- paste(rob_letters, rob_names, sep = ": ")
      rob_legend_y <- (4:(n_rob + 3))
      legend_width_user <- max(strwidth(rob_legend))
      spacer_width_user <- strwidth("••")

      # legend for domains
      mtext(rob_legend, side = 1, line = rob_legend_y, las = 1, at = -longest_label_user, adj = 0)

      # legend for colours
      rob_levels <- c("Low risk of bias", "Some concerns", "High risk of bias")
      mtext(rep("•", 3), side = 1, line = (4:6) - 0.25, las = 1, at = -(longest_label_user - legend_width_user - spacer_width_user), adj = 0, padj = 0.5, cex = 3, col = palette)
      mtext(rob_levels, side = 1, line = (4:6) - 0.25, las = 1, at = -(longest_label_user - legend_width_user - (3 * spacer_width_user)), adj = 0, padj = 0.5)

    }


    for (row in 1:length(pairwise_treatments[[1]])) {
      .AddTreatmentEffectBlock(
        pairwise = pairwise,
        outcome_type = outcome_type,
        max_label_width = longest_label,
        rob_x_position = start_rob,
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
  height = plot_height) |> crop_svg()
}

#' Finds suitable starting limits for the x-axis.
#'
#' @param pairwise Output from meta::pairwise().
#' @return Named vector in the form c(lower, upper).
FindStartingXLimits <- function(pairwise) {
  #The lowest confidence interval bound
  lower <- min(pairwise$TE - 1.96 * pairwise$seTE)
  #The highest confidence interval bound
  upper <- max(pairwise$TE + 1.96 * pairwise$seTE)
  #Suitable x-axis limits and ticks
  x_ticks <- .FindXTicks(lower = lower, upper = upper)
  return(c(lower = x_ticks[1],  upper = x_ticks[length(x_ticks)]))
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
  increment <- 10 ^ floor(log10(difference))
  #Tweak the increment
  if (difference / increment < 3) {
    increment <- increment / 2
  } else if (difference / increment > 7) {
    increment <- increment * 2
  }
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
#' @param outcome_type "binary" or "continuous".
#' @param y_position y co-ordinate for the effect size and confidence interval.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
.AddOutcomeText <- function(pairwise, outcome_type, y_position, studlab, treatment1, treatment2) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  point_estimate <- pairwise$TE[row]
  ci_lower <- pairwise$TE[row] - 1.96 * pairwise$seTE[row]
  ci_upper <- pairwise$TE[row] + 1.96 * pairwise$seTE[row]
  if (outcome_type == "binary") {
    point_estimate <- exp(point_estimate)
    ci_lower <- exp(ci_lower)
    ci_upper <- exp(ci_upper)
  }
  point_estimate <- point_estimate |> sprintf(fmt = "%-.2f")
  ci_lower <- ci_lower |> sprintf(fmt = "%-.2f")
  ci_upper <- ci_upper |> sprintf(fmt = "%-.2f")

  mtext(
    text = paste0(point_estimate, " (", ci_lower, ", ", ci_upper,  ")"),
    side = 4,
    line = 1,
    at = y_position,
    las = 1,
    adj = 0
  )
}



#' Add risk of bias circles for a single comparison in a single study.
#'
#' @param pairwise Output from meta::pairwise().
#' @param outcome_type "binary" or "continuous".
#' @param x_position x co-ordinate for the first circle.
#' @param y_position y co-ordinate for the circles.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
#' @param palette Vector of colours to use
.AddRiskOfBias <- function(pairwise, outcome_type, x_position, y_position, studlab, treatment1, treatment2, rob_variables, palette) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  colours <- palette
  start_x <- x_position

  mtext("•",
        side = 4,
        at = y_position,
        line = start_x:(start_x + length(rob_variables) - 1),
        las = 1,
        adj = 0.5,
        col = colours[sapply(pairwise[row, rob_variables], function(x){x[[1]]})],
        cex = 3
      )
}


#' Add the treatment effect lines, confidence intervals, studies, and comparison header, for a single comparison.
#'
#' @param pairwise Output from meta::pairwise().
#' @param outcome_type "binary" or "continuous".
#' @param max_label_width
#' @param rob_x_position
#' @param y_header_position y co-ordinate for the header.
#' @param y_positions Vector of y co-ordinates for the study labels and confidence intervals.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
#' @param palette Vector of colours to use
.AddTreatmentEffectBlock <- function(pairwise, outcome_type, max_label_width, rob_x_position, y_header_position, y_positions, treatment1, treatment2, rob_variables, palette) {

  # header
  mtext(
    text = paste0(treatment1, " vs. ", treatment2),
    side = 2,
    line = max_label_width,
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
    line = max_label_width - 1,
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
      outcome_type = outcome_type,
      y_position = y_positions[row],
      studlab = pairwise_subset$studlab[row],
      treatment1 = pairwise_subset$treat1[row],
      treatment2 = pairwise_subset$treat2[row]
    )
    if (include_rob) {
      .AddRiskOfBias(
        pairwise = pairwise,
        x_position = rob_x_position,
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


#' Function to determine the height of the forest plot.
#'
#' @param pairwise Output from meta::pairwise().
#' @param treatment_order Vector of the treatments in the order they should appear in the forest plot.
#' @return Forest plot height in pixels.
# ForestPlotHeight <- function(pairwise, treatment_order) {
#   pairwise_treatments <- PairwiseTreatments(
#     pairwise = pairwise,
#     treatment_order = treatment_order
#   )
#   return(max(400, 20 * max(pairwise_treatments$y_position_last)))
# }

