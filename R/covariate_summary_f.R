#' Produce a plot summarising the covariate value for each study arm
#'
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
covariate_summary <- function(configured_data, logger = NULL){
  check_param_classes(c("configured_data"),
                      c("configured_data"), logger)

  if (!any(grepl("covar\\.", names(configured_data$connected_data)))){
    logger |> asyncLog(type = "error", "The data does not contain a covariate column")
    return()
  }

  if (FindDataShape(configured_data$connected_data) == "wide") {
    long_data <- as.data.frame(WideToLong(configured_data$connected_data, outcome = configured_data$outcome))
  } else if (FindDataShape(configured_data$connected_data) == "long") {
    long_data <- configured_data$connected_data
  }

  # Add column with treatment labels
  # x$ and y$ syntax is used instead of .data$ as they are in different df
  mutated_data <- long_data |>
    dplyr::inner_join(configured_data$treatments, by = dplyr::join_by(x$T == y$Number))

  covariate_column <- configured_data$covariate$column
  covariate_name <- stringr::str_to_sentence(configured_data$covariate$name)
  caption_setting <- paste("Covariate", configured_data$covariate$name) # Text to add to caption

  # sort by treatment and calculate y positions
  df <- mutated_data |>
    dplyr::arrange(dplyr::desc(.data$Label), .data$Study)
  df$y_position <- baseline_summary_y_position(df$Label)
  label_y_position <- c(df$y_position[diff(df$y_position) > 1], max(df$y_position)) + 1.2

  inches_to_lines <- par("cin")[2]

  # find the longest label in lines
  longest_treatment_label_inches <- max(strwidth(df$Label, units = "in"))
  longest_study_label_inches <- max(strwidth(df$Study, units = "in")) + inches_to_lines # + 1 for offsetting
  label_width_inches <- max(longest_treatment_label_inches, longest_study_label_inches)
  label_width <- label_width_inches / inches_to_lines

  # set plot dimensions and margins
  top_line <- max(df$y_position)

  bottom_margin <- 12
  top_margin <- 4
  left_margin <- 4 + (label_width_inches + (2 * inches_to_lines)) / inches_to_lines
  right_margin <- 4

  plot_area_width <- 6

  plot_height <- (bottom_margin + top_margin + top_line) * inches_to_lines

  plot_width <- label_width_inches +
    (2 * inches_to_lines) +
    plot_area_width

  # get nice values for axis
  pretty_axis <- pretty(df[[covariate_column]])

  svglite::xmlSVG({
    par(family = "Arimo")
    par(mar = c(bottom_margin, left_margin, top_margin, right_margin))

    plot(
      x = df[[covariate_column]],
      y = df$y_position,
      pch = 18,
      xlim = c(min(pretty_axis), max(pretty_axis)),
      ylim = c(0, max(df$y_position) + 2),
      xaxt = "n",
      yaxt = "n",
      ylab = "",
      xlab = covariate_name,
      frame.plot = F, # remove border
      yaxs = "i" # remove internal padding
    )

    title("Covariate values", line = 2)
    # study labels
    mtext(df$Study, side = 2, at = df$y_position, las = 2, line = label_width - 1, adj = 0)
    # treatment labels
    mtext(unique(df$Label), side = 2, at = label_y_position, las = 2, font = 2, line = label_width, adj = 0)
    # mean line
    abline(v = mean(df[[covariate_column]]), lty = 2)
    # x-axis
    axis(side = 1, at = pretty(df[[covariate_column]]))

    # legend
    mtext(
      c("The plotted value is the same for all treatment arms across a study"),
      side = 2,
      line = label_width - 1,
      las = 1,
      at = -6,
      adj = 0
    )

  },
  width = plot_width,
  height = plot_height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

}
#' Produce a plot summarising the covariate value for each study arm
#'
#' @inheritParams common_params
#' @inherit return-svg return
#' @import ggplot2
#' @export
covariate_summary_old <- function(configured_data, logger = NULL){

  check_param_classes(c("configured_data"),
                      c("configured_data"), logger)

  if (!any(grepl("covar\\.", names(configured_data$connected_data)))){
    logger |> asyncLog(type = "error", "The data does not contain a covariate column")
    return()
  }

  if (FindDataShape(configured_data$connected_data) == "wide") {
    long_data <- as.data.frame(WideToLong(configured_data$connected_data, outcome = configured_data$outcome))
  } else if (FindDataShape(configured_data$connected_data) == "long") {
    long_data <- configured_data$connected_data
  }

  caption_setting <- paste("Covariate", configured_data$covariate$name) # Text to add to caption

  # Use FindCovariateName function to find the covariate column
  covariate <- FindCovariateNames(long_data)[1]

  # Use GetFriendlyCovariateName function to label the y-axis
  y_axis_label <- GetFriendlyCovariateName(covariate)

  # Add column with treatment labels
  # x$ and y$ syntax is used instead of .data$ as they are in different df
  mutated_data <- long_data |>
    dplyr::inner_join(configured_data$treatments, by = dplyr::join_by(x$T == y$Number))

  # Convert tibble created by dplyr to df
  BUGSnet_df <- as.data.frame(mutated_data)

  # BUGSnet data prep to convert data to format required for data.plot
  BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet_df,
                                     varname.t = 'Label',
                                     varname.s = 'Study')

  plot <- BUGSnet::data.plot(BUGSnet_data,
                             covariate = covariate,
                             by = 'treatment',
                             text.size = 16,
                             coord.flip = TRUE)

  # Add caption text under plot
  plot <- plot +
    labs(caption = PasteCaptionText(caption_setting))


  plot <- plot +
    theme(plot.caption = element_text(hjust = 1)) + # Right aligned caption text
    # Centre x-axis text & increase spacing between tick and axis labels
    theme(axis.text.x = element_text(angle = 1, vjust = 0, hjust = 0, margin = margin(b = 10))) +
    ylab(y_axis_label)

  n_arms <- nrow(long_data)
  plot_height <- ifelse(n_arms <= 20, 400, 400 + 15 * (n_arms - 20)) / 72

  svg <- svglite::xmlSVG({
    # necessary for ggplot
    print(plot)
  },
  width = 8,
  height = plot_height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)

}

