#' Produce a plot summarising the covariate value for each study arm
#'
#' @inheritParams common_params
#' @inherit return-svg return
#' @import ggplot2
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

