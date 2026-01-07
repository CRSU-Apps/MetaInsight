#' @title baseline_summary
#' @description Creates a plot of the covariate value for each study arm
#' @inheritParams common_params
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @import ggplot2
#' @export
covariate_summary <- function(connected_data, outcome, treatment_df, logger = NULL){

  check_param_classes(c("connected_data", "outcome", "treatment_df"),
                      c("data.frame", "character", "data.frame"), logger)

  if (!any(grepl("covar\\.", names(connected_data)))){
    logger |> asyncLog(type = "error", "connected_data does not contain a covariate column")
    return()
  }

  if (!outcome %in% c("Binary", "Continuous")){
    logger |> writeLog(type = "error", "outcome must be 'Binary' or 'Continuous'")
    return()
  }

  if (FindDataShape(connected_data) == "wide") {
    long_data <- as.data.frame(WideToLong(connected_data, outcome = outcome))
  } else if (FindDataShape(connected_data) == "long") {
    long_data <- connected_data
  }

  caption_setting <- "covariate" # Text to add to caption

  # Use FindCovariateName function to find the covariate column
  covariate <- FindCovariateNames(long_data)[1]

  # Use GetFriendlyCovariateName function to label the y-axis
  y_axis_label <- GetFriendlyCovariateName(covariate)

  # Add column with treatment labels
  # x$ and y$ syntax is used instead of .data$ as they are in different df
  mutated_data <- long_data |>
    dplyr::inner_join(treatment_df, by = dplyr::join_by(x$T == y$Number))

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

