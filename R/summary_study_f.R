#' Produce a forest plot of pairwise comparisons, grouped by treatment pairs.
#' If risk of bias data was loaded these are also included.
#'
#' @inheritParams common_params
#' @param plot_area_width numeric. The width of the plot area containing the
#' treatment effects in inches. Defaults to `6`.
#' @param colourblind logical. Whether to use a colourblind-friendly palette. Defaults to `FALSE`
#' @param x_min numeric. Minimum value for the x-axis. Defaults to `NULL`. For binary outcomes
#' values should be wrapped in `log()`
#' @param x_max numeric. Maximum value for the x-axis. Defaults to `NULL`. For binary outcomes
#' values should be wrapped in `log()`
#' @param interactive logical. Whether the plot should be altered for preparation
#' into an interactive interface. Defaults to `FALSE`
#' @inherit return-svg return
#' @import graphics
#' @export
summary_study <- function(configured_data, plot_area_width = 6, colourblind = FALSE, x_min = NULL, x_max = NULL, interactive = FALSE, logger = NULL) {

  check_param_classes(c("configured_data","plot_area_width", "colourblind"),
                      c("configured_data", "numeric", "logical"), logger)

  if (plot_area_width < 6 || plot_area_width > 20){
    logger |> writeLog(type = "error", "plot_area_width must be between 6 and 20")
    return()
  }

  rob_data_frame <- unique(configured_data$connected_data[, c("Study", FindRobNames(configured_data$connected_data))])
  if (!is.data.frame(rob_data_frame)) {
    pairwise <- configured_data$freq$d1
  } else {
    pairwise <- as.data.frame(merge(configured_data$freq$d1, rob_data_frame, by = "Study"))
  }

  if (is.null(x_min) || is.null(x_max)){
    min_max <- summary_study_min_max(pairwise, configured_data$outcome)
    x_min <- min_max[1]
    x_max <- min_max[2]
  }

  # need to check these after checking they are not NULL
  check_param_classes(c("x_min", "x_max"),
                      c("numeric", "numeric"), logger)

  pairwise_treatments <- PairwiseTreatments(
    pairwise = pairwise,
    treatment_order = configured_data$freq$lstx
  )

  if (colourblind) {
    palette  <- c("#fed98e", "#fe9929", "#d95f0e")
  } else {
    palette <- c("chartreuse3", "darkgoldenrod1", "red")
  }

  rob_variables <- FindRobNames(pairwise)
  rob_names <- gsub("rob.", "", FindRobNames(configured_data$connected_data))
  n_rob_variables <- length(rob_variables)

  x_label <- switch(
    configured_data$outcome_measure,
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
  if (is.element(configured_data$outcome_measure, c("OR", "RR"))) {
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
    if (is.element(configured_data$outcome_measure, c("OR", "RR"))) {
      x_labels <- signif(exp(x_ticks), digits = 2)
    } else if (is.element(configured_data$outcome_measure, c("MD", "SMD", "RD"))) {
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
      formatted_rob_names <- gsub("_", " ", rob_names) |> stringr::str_to_sentence()
      rob_legend <- paste(rob_letters, formatted_rob_names, sep = ": ")
      rob_legend_y <- -(3:(n_rob_variables + 2))
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
        # bullet point unicode
        rep("\u2022", 3),
        side = 2,
        line = legend_x_position - legend_width - 1,
        las = 1,
        at = -3:-5,
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
        at = -3:-5,
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
        palette = palette,
        interactive = interactive
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
#' @param pairwise Output from `meta::pairwise()`.
#' @param y_position y co-ordinate for the study label and confidence interval.
#' @param studlab Study.
#' @param treatment1 First treatment in the comparison.
#' @param treatment2 Second treatment in the comparison.
#' @param ci_limit_height Height of the vertical lines at the end of the confidence intervals. Defaults to 0.3.
#' @importFrom graphics points lines
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
  graphics::mtext(
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

  graphics::mtext(
    # bullet point unicode
    "\u2022",
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
#' @param interactive Whether the plot will be interactive
.AddTreatmentEffectBlock <- function(pairwise,
                                     label_x_position,
                                     outcome_x_position,
                                     rob_x_positions,
                                     y_header_position,
                                     y_positions,
                                     treatment1,
                                     treatment2,
                                     rob_variables,
                                     palette,
                                     interactive) {

  # header
  graphics::mtext(
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
  graphics::mtext(
    text = pairwise_subset$studlab,
    side = 2,
    line = label_x_position - 1,
    at = y_positions,
    las = 1,
    adj = 0
  )

  include_rob <- length(rob_variables) > 0

  for (row in 1:length(pairwise_subset$studlab)) {

    if (interactive){
      # add rectangles for partitioning later
      graphics::rect(-100, y_positions[row] - 0.5, 100, y_positions[row] + 0.5, col = "#00000000")
    }

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
#' @inheritParams common_params
#' @return Vector of xmin and xmax
#' @export
summary_study_min_max <- function(pairwise, outcome){

  log.scale <- ifelse(outcome == "binary", TRUE, FALSE)

  all_min <- (pairwise$TE - 1.96 * pairwise$seTE)
  all_max <- (pairwise$TE + 1.96 * pairwise$seTE)

  x_min <- format_xlim(min(all_min, na.rm = TRUE), "min", log.scale)
  x_max <- format_xlim(max(all_max, na.rm = TRUE), "max", log.scale)

  c(x_min, x_max)
}

#' Produce an interactive version of the summary_study plot for use in the
#' interface for excluding studies
#'
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
summary_study_interactive <- function(configured_data){

  initial <- summary_study(configured_data, plot_area_width = 6, interactive = TRUE)
  svg_doc <- xml2::read_xml(initial)

  # Get viewBox dimensions
  svg_node <- xml2::xml_find_first(svg_doc, "//d1:svg", ns = c(d1 = "http://www.w3.org/2000/svg"))
  viewbox <- xml2::xml_attr(svg_node, "viewBox")
  values <- strsplit(viewbox, " ")[[1]] |> as.numeric()
  viewbox_x <- values[1]
  viewbox_width <- values[3]

  # add to svg id to allow targeting later
  xml2::xml_set_attr(svg_node, "id", "summary_exclude_interface")

  # Find all rect elements with stroke-width: 0.75
  rects <- xml2::xml_find_all(
    svg_doc,
    ".//d1:rect[contains(@style, 'stroke-width: 0.75')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Get rect y bounds
  rect_bounds <- lapply(rects, function(rect) {
    list(
      y_min = as.numeric(xml2::xml_attr(rect, "y")),
      y_max = as.numeric(xml2::xml_attr(rect, "y")) + as.numeric(xml2::xml_attr(rect, "height")),
      height = as.numeric(xml2::xml_attr(rect, "height")),
      element = rect
    )
  })

  # Find all other elements (not rects with stroke-width 0.75, not in defs)
  all_elements <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g/*[not(self::d1:rect[contains(@style, 'stroke-width: 0.75')])]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Helper function to get y coordinates from an element
  get_element_y_coords <- function(elem) {
    elem_name <- xml2::xml_name(elem)
    y_coords <- c()

    if (elem_name == "text") {
      y_coords <- as.numeric(xml2::xml_attr(elem, "y"))
    } else if (elem_name == "line") {
      y_coords <- c(as.numeric(xml2::xml_attr(elem, "y1")),
                    as.numeric(xml2::xml_attr(elem, "y2")))
    } else if (elem_name %in% c("polyline", "polygon")) {
      points <- xml2::xml_attr(elem, "points")
      points_clean <- gsub(",", " ", points)
      vals <- as.numeric(strsplit(trimws(points_clean), "\\s+")[[1]])
      y_coords <- vals[seq(2, length(vals), 2)]
    } else if (elem_name == "rect") {
      y <- as.numeric(xml2::xml_attr(elem, "y"))
      height <- as.numeric(xml2::xml_attr(elem, "height"))
      y_coords <- c(y, y + height)
    }

    return(y_coords)
  }

  # Helper function to check if element overlaps with rect's y range
  is_inside_rect <- function(elem, rect_bound) {
    y_coords <- get_element_y_coords(elem)

    if (length(y_coords) == 0 || all(is.na(y_coords))) {
      return(FALSE)
    }

    y_min <- min(y_coords, na.rm = TRUE)
    y_max <- max(y_coords, na.rm = TRUE)

    # Check if element's y range overlaps with rect's y range
    return(y_min <= rect_bound$y_max && y_max >= rect_bound$y_min)
  }

  # Find parent group to add new groups to
  parent_group <- xml2::xml_find_first(
    svg_doc,
    ".//d1:g",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Track which elements have been assigned
  assigned_elements <- c()

  # Group elements by which rect they belong to
  for (i in seq_along(rect_bounds)) {
    rect_bound <- rect_bounds[[i]]

    # Create a new group element
    new_group <- xml2::xml_add_child(parent_group, "g")

    # Create a new rect with updated dimensions and style
    new_rect <- xml2::xml_add_child(new_group, "rect")
    xml2::xml_attr(new_rect, "x") <- viewbox_x
    xml2::xml_attr(new_rect, "y") <- rect_bound$y_min
    xml2::xml_attr(new_rect, "width") <- viewbox_width
    xml2::xml_attr(new_rect, "height") <- rect_bound$height
    xml2::xml_attr(new_rect, "style") <- "stroke: none; opacity: 0.0; fill:red"

    # Find elements inside this rect's y range and collect text content
    first_text_content <- NULL

    for (j in seq_along(all_elements)) {
      if (!(j %in% assigned_elements)) {
        elem <- all_elements[[j]]
        if (is_inside_rect(elem, rect_bound)) {
          # Copy the element without namespace
          elem_copy <- xml2::xml_new_root(xml2::xml_name(elem))

          # Copy all attributes
          attrs <- xml2::xml_attrs(elem)
          for (attr_name in names(attrs)) {
            xml2::xml_attr(elem_copy, attr_name) <- attrs[[attr_name]]
          }

          # Copy text content if it's a text element
          if (xml2::xml_name(elem) == "text") {
            xml2::xml_text(elem_copy) <- xml2::xml_text(elem)
          }

          # Add to group
          xml2::xml_add_child(new_group, elem_copy)
          assigned_elements <- c(assigned_elements, j)

          # Capture first text element's content for class name
          if (is.null(first_text_content) && xml2::xml_name(elem) == "text") {
            first_text_content <- xml2::xml_text(elem)
          }
        }
      }
    }

    # Add class attribute based on first text element
    if (!is.null(first_text_content)) {
      class_name <- gsub("[^A-Za-z0-9_-]", "_", first_text_content)
      xml2::xml_attr(new_group, "class") <- class_name
    }

    # Add ID
    xml2::xml_attr(new_group, "id") <- paste0("line", i)
  }

  # Remove original rects
  xml2::xml_remove(rects)

  # Remove assigned elements
  if (length(assigned_elements) > 0) {
    xml2::xml_remove(all_elements[assigned_elements])
  }

  paste(svg_doc, collapse = "\n") |> HTML()

}
