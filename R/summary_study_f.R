#' Create a forest plot of pairwise comparisons, grouped by treatment pairs.
#'
#' @param pairwise Output from meta::pairwise().
#' @param treatment_order Vector of the treatments in the order they should appear in the forest plot.
#' @param outcome_measure One of "MD", "SMD", "OR", "RR", or "RD".
#' @param x_limits Named vector of x-axis limits in the form (lower, upper).
#' @export
#' @return Forest plot.
summary_study <- function(data, freq, outcome_measure, x_limits) {

  rob_data_frame <- unique(data[, c("Study", FindRobNames(data))])
  if (!is.data.frame(rob_data_frame)) {
    pairwise <- freq$d1
  } else {
    pairwise <- merge(freq$d1, rob_data_frame, by = "Study")
  }

  pairwise_treatments <- PairwiseTreatments(
    pairwise = pairwise,
    treatment_order = freq$lstx
  )

  x_width <- unname(x_limits["upper"] - x_limits["lower"])
  x_ticks <- .FindXTicks(lower = x_limits["lower"], upper = x_limits["upper"])

  rob_variables <- FindRobNames(pairwise)
  short_rob_names <- ShortenRobNames(rob_variables)
  n_rob_variables <- length(rob_variables)

  #Enlarge the x-axis to allow the labels to go on the left and the outcomes on the right
  left_enlargement_factor <- 0.04 * (max(nchar(as.character(pairwise$treat1))) + max(nchar(as.character(pairwise$treat2))) + 4)
  outcome_text_factor <-  0.9
  per_rob_factor <- 0.2
  right_enlargement_factor <- outcome_text_factor + per_rob_factor * n_rob_variables
  enlarged_x_limits <- x_limits - c(left_enlargement_factor * x_width, 0) + c(0, right_enlargement_factor * x_width)

  #x-position for the within-study comparison-level outcomes
  x_outcome_position <- x_limits["upper"] + 0.1 * x_width
  #x-position for the risk of bias
  if (n_rob_variables > 0) {
    x_rob_positions <- seq(
      from = x_limits["upper"] + (outcome_text_factor + per_rob_factor) * x_width,
      to = x_limits["upper"] + (outcome_text_factor + per_rob_factor * n_rob_variables) * x_width,
      by = per_rob_factor * x_width
    )
  }

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

  par(family = "Arimo")
  par(mar = c(8, 0, 0, 0))

  plot(
    x = NA,
    y = NA,
    xlim = enlarged_x_limits,
    ylim = c(0, max(pairwise_treatments$y_position_last) + 2),
    yaxt = "n",
    xaxt = "n",
    ylab = "",
    xlab = x_label
  )

  #Define the ticks and labels for the x-axis
  if (outcome_type == "binary") {
    x_labels <- signif(exp(x_ticks), digits = 1)
  } else if (outcome_type == "continuous") {
    x_labels <- x_ticks
  }

  #Add the x-axis
  axis(
    side = 1,
    at = x_ticks,
    labels = x_labels
  )

  #Add the line of no effect
  lines(
    x = rep(0, times = 2),
    y = c(0, max(pairwise_treatments$y_position_last)),
    lty = 2
  )

  #Add header of the within-study comparison-level outcomes
  text(
    x = x_outcome_position,
    y = max(pairwise_treatments$y_position_last) + 1,
    labels = paste0(x_label, " (95% CI)"),
    adj = 0,
    font = 2
  )

  #Add risk of bias header
  if (n_rob_variables > 0) {
    text(
      x = x_rob_positions - 0.25 * per_rob_factor * x_width,
      y = max(pairwise_treatments$y_position_last) + 1,
      labels = short_rob_names,
      adj = 0,
      font = 2
    )
    if (is.element("rob", rob_variables)) {
      rob_position <- which(rob_variables == "rob")
      lines(
        x = rep(x_rob_positions[rob_position] - 0.5 * per_rob_factor * x_width, times = 2),
        y = c(1, max(pairwise_treatments$y_position_last) + 1),
        lty = 2,
        col = "azure4"
      )
    }
  }

  # Add legend
  mtext("rob1: balbalbla", side = 1, line = 2.5, cex = 1, adj = 0.05)
  mtext("rob2: dudada", side = 1, line = 3.5, cex = 1, adj = 0.05)
  mtext("rob: Overall risk of bias", side = 1, line = 4.5, cex = 1, adj = 0.05)
  mtext("ind: Indirectness", side = 1, line = 5.5, cex = 1, adj = 0.05)

  for (row in 1:length(pairwise_treatments[[1]])) {
    .AddTreatmentEffectBlock(
      pairwise = pairwise,
      outcome_type = outcome_type,
      x_label_position = enlarged_x_limits[1],
      x_outcome_position = x_outcome_position,
      x_rob_positions = x_rob_positions,
      y_header_position = pairwise_treatments$y_position_last[row],
      y_positions = (pairwise_treatments$y_position_first[row] + 1):(pairwise_treatments$y_position_last[row] - 1),
      treatment1 = pairwise_treatments$treat1[row],
      treatment2 = pairwise_treatments$treat2[row],
      rob_variables = rob_variables
    )
  }
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
  #Point estimate
  points(
    x = pairwise$TE[row],
    y = y_position,
    pch = 18
  )
  #Horizontal line
  lines(
    x = c(pairwise$TE[row] - 1.96 * pairwise$seTE[row], pairwise$TE[row] + 1.96 * pairwise$seTE[row]),
    y = rep(y_position, times = 2)
  )
  #Vertical lines
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
#' @param x_outcome_position x co-ordinate for the comparison-within-study outcomes.
#' @param y_position y co-ordinate for the effect size and confidence interval.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
.AddOutcomeText <- function(pairwise, outcome_type, x_outcome_position, y_position, studlab, treatment1, treatment2) {
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
  text(
    x = x_outcome_position,
    y = y_position,
    labels = paste0(point_estimate, " (", ci_lower, ", ", ci_upper,  ")"),
    adj = 0
  )
}



#' Add risk of bias circles for a single comparison in a single study.
#'
#' @param pairwise Output from meta::pairwise().
#' @param outcome_type "binary" or "continuous".
#' @param x_rob_positions Vector of x co-ordinates for the circles.
#' @param y_position y co-ordinate for the circles.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
.AddRiskOfBias <- function(pairwise, outcome_type, x_rob_positions, y_position, studlab, treatment1, treatment2, rob_variables) {
  row <- which(pairwise$studlab == studlab & pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2)
  colours <- c("chartreuse3", "darkgoldenrod1", "red")

  points(
    x = x_rob_positions,
    y = rep(y_position, times = length(x_rob_positions)),
    pch = 19,
    col = colours[sapply(pairwise[row, rob_variables], function(x){x[[1]]})],
    cex = 2
  )
}


#' Add the treatment effect lines, confidence intervals, studies, and comparison header, for a single comparison.
#'
#' @param pairwise Output from meta::pairwise().
#' @param outcome_type "binary" or "continuous".
#' @param x_label_position x co-ordinate for the header and study labels.
#' @param x_outcome_position x co-ordinate for the comparison-within-study outcomes.
#' @param x_rob_positions Vector of x co-ordinates for the circles.
#' @param y_header_position y co-ordinate for the header.
#' @param y_positions Vector of y co-ordinates for the study labels and confidence intervals.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param rob_variables Vector of RoB and indirectness variable names from 'pairwise'.
.AddTreatmentEffectBlock <- function(pairwise, outcome_type, x_label_position, x_outcome_position, x_rob_positions, y_header_position, y_positions, treatment1, treatment2, rob_variables) {
  #Header
  text(
    x = x_label_position,
    y = y_header_position,
    labels = paste0(treatment1, " vs. ", treatment2),
    adj = 0,
    font = 2
  )

  #Select the rows comparing to the two treatments
  pairwise_subset <- pairwise[
    pairwise$treat1 == treatment1 & pairwise$treat2 == treatment2
    ,
  ]

  #Study labels
  text(
    x = x_label_position,
    y = y_positions,
    labels = pairwise_subset$studlab,
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
      x_outcome_position = x_outcome_position,
      y_position = y_positions[row],
      studlab = pairwise_subset$studlab[row],
      treatment1 = pairwise_subset$treat1[row],
      treatment2 = pairwise_subset$treat2[row]
    )
    if (include_rob) {
      .AddRiskOfBias(
        pairwise = pairwise,
        x_rob_positions = x_rob_positions,
        y_position = y_positions[row],
        studlab = pairwise_subset$studlab[row],
        treatment1 = pairwise_subset$treat1[row],
        treatment2 = pairwise_subset$treat2[row],
        rob_variables = rob_variables
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

