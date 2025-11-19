#' @title baseline_summary
#' @description Creates a plot of baselink risk for each study arm
#' @param connected_data dataframe. Input data set created by `setup_configure()` or `setup_exclude`
#' @param treatment_df dataframe containing the treatment ID ('Number') and the treatment name ('Label').
#' @param outcome character. The type of outcome being measured either `Continuous` or `Binary`
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @import ggplot2
#' @export
baseline_summary <- function(connected_data, outcome, treatment_df, logger = NULL){

  check_param_classes(c("connected_data", "outcome", "treatment_df"),
                      c("data.frame", "character", "data.frame"), logger)

  if (!outcome %in% c("Binary", "Continuous")){
    logger |> writeLog(type = "error", "outcome must be 'Binary' or 'Continuous'")
    return()
  }

  if (FindDataShape(connected_data) == "wide") {
    long_data <- as.data.frame(WideToLong(connected_data, outcome = outcome))
  } else if (FindDataShape(connected_data) == "long") {
    long_data <- connected_data
  }

  if (outcome == "Continuous") {

    # Add baseline column that is the mean value and
    # baseline_error column that is 1.96 * SD / sqrt(N)
    # of the reference arm for the study, or NA if there is no reference arm
    # Reference arm is always numbered 1 internally
    mutated_data <- long_data |>
      dplyr::group_by(Study) |>
      dplyr::mutate(
        baseline = ifelse(is.null(Mean[T == 1]), NA, Mean[T == 1])
      ) |>
      dplyr::mutate(
        baseline_error = ifelse(is.null(Mean[T == 1]), NA, (1.96 * SD[T == 1]) / sqrt(N[T == 1]))
      )

    # Error bar text for continuous outcomes
    error_bar_text <- "Error bars: mean +/- 1.96 * SD / sqrt(N)"

  } else if (outcome == "Binary") {

    # Use escalc function with the logit transformed proportion (PLO) measure
    effects <- metafor::escalc(measure = "PLO", xi = long_data$R, ni = long_data$N)

    # Merge effects and long_data into one df
    mutated_data <- cbind(long_data, effects)

    # Add baseline column that is yi column from escalc
    # baseline_error column that is 1.96 * sqrt(vi) column from escalc
    # of the reference arm for the study, or NA if there is no reference arm
    # Reference arm is always numbered 1 internally
    mutated_data <- mutated_data |>
      dplyr::group_by(Study) |>
      dplyr::mutate(
        baseline = ifelse(is.null(R[T == 1]), NA, yi[T == 1])
      ) |>
      dplyr::mutate(
        baseline_error = ifelse(is.null(R[T == 1]), NA, 1.96 * sqrt(vi[T == 1]))
      )

    # Error bar text for binary outcomes
    error_bar_text <- "Error bars: logit(outcome) +/- 1.96 * se(logit(outcome))"

  }

  # these three calls could be converted to a function to use in covariate_summary too
  # Add column with treatment labels
  mutated_data <- mutated_data |>
    dplyr::inner_join(treatment_df, by = dplyr::join_by(T == Number))

  # Convert tibble created by dplyr to df
  BUGSnet_df <- as.data.frame(mutated_data)

  # BUGSnet data prep to convert data to format required for data.plot
  BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet_df,
                                     varname.t = 'Label',
                                     varname.s = 'Study')

  plot <- BUGSnet::data.plot(BUGSnet_data,
                             covariate = "baseline",
                             half.length = "baseline_error",
                             by = 'treatment',
                             text.size = 16,
                             coord.flip = TRUE)

  # Add caption text under plot
  plot <- plot +
    labs(caption = PasteCaptionText("baseline risk", error_bar_text))

  if (outcome == "Binary") {

    # Plot in logit scale, label on probability scale
    plot <- plot +
      scale_y_continuous(labels = function(x) signif(plogis(x), digits = 2))
  }

  plot <- plot +
    theme(plot.caption = element_text(hjust = 1)) + # Right aligned caption text
    # Centre x-axis text & increase spacing between tick and axis labels
    theme(axis.text.x = element_text(angle = 1, vjust = 0, hjust = 0, margin = margin(b = 10))) +
    ylab("Baseline risk")

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

#' Paste the caption text together
#'
#' @param plot_type Text string to describe type of plot. Can be "baseline risk" or "covariate"
#' @param error_bar_text Text string to explain the error bar (optional)
#' @return Text string to be used for caption

PasteCaptionText <- function(caption_setting, error_bar_text = NULL) {

  caption_text <- paste('The plotted', caption_setting, 'value is the same for all treatment arms across
                              a study')

  if(caption_setting == "baseline risk") {
    caption_text <- paste(caption_text, 'and is the outcome in the reference treatment arm.
                                Values for studies without a reference treatment arm are not plotted. \n',
                          error_bar_text)
  }

  return(caption_text)

}

