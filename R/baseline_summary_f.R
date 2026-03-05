#' @title Summarise baseline risk
#' @description Produce a plot summarising baselink risk for each study arm
#'
#' @inheritParams common_params
#' @import graphics
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' baseline_summary(configured_data)
#'
#' @export
baseline_summary <- function(configured_data, logger = NULL){

  check_param_classes(c("configured_data"),
                      c("configured_data"), logger)

  if (!configured_data$outcome_measure %in% c("OR", "MD")){
    logger |> writeLog(type = "error", "configured data must have an outcome_measure of 'OR' or 'MD'")
    return()
  }

  if (FindDataShape(configured_data$connected_data) == "wide") {
    long_data <- as.data.frame(WideToLong(configured_data$connected_data, outcome = configured_data$outcome))
  } else if (FindDataShape(configured_data$connected_data) == "long") {
    long_data <- configured_data$connected_data
  }

  if (configured_data$outcome_measure == "MD") {

    # Add baseline column that is the mean value and
    # baseline_error column that is 1.96 * SD / sqrt(N)
    # of the reference arm for the study, or NA if there is no reference arm
    # Reference arm is always numbered 1 internally
    mutated_data <- long_data |>
      dplyr::group_by(.data$Study) |>
      dplyr::mutate(
        baseline = ifelse(is.null(.data$Mean[.data$T == 1]), NA, .data$Mean[.data$T == 1])
      ) |>
      dplyr::mutate(
        baseline_error = ifelse(is.null(.data$Mean[.data$T == 1]), NA, (1.96 * .data$SD[.data$T == 1]) / sqrt(.data$N[.data$T == 1]))
      )

    # Error bar text for continuous outcomes
    error_bar_text <- "Error bars: mean +/- 1.96 * SD / sqrt(N)"

  } else if (configured_data$outcome_measure == "OR") {

    # Use escalc function with the logit transformed proportion (PLO) measure
    effects <- metafor::escalc(measure = "PLO", xi = long_data$R, ni = long_data$N)

    # Merge effects and long_data into one df
    mutated_data <- cbind(long_data, effects)

    # Add baseline column that is yi column from escalc
    # baseline_error column that is 1.96 * sqrt(vi) column from escalc
    # of the reference arm for the study, or NA if there is no reference arm
    # Reference arm is always numbered 1 internally
    mutated_data <- mutated_data |>
      dplyr::group_by(.data$Study) |>
      dplyr::mutate(
        baseline = ifelse(is.null(.data$R[.data$T == 1]), NA, .data$yi[.data$T == 1])
      ) |>
      dplyr::mutate(
        baseline_error = ifelse(is.null(.data$R[.data$T == 1]), NA, 1.96 * sqrt(.data$vi[.data$T == 1]))
      )

    # Error bar text for binary outcomes
    error_bar_text <- "Error bars: logit(outcome) +/- 1.96 * se(logit(outcome))"

  }

  # these three calls could be converted to a function to use in covariate_summary too
  # Add column with treatment labels
  # x$ and y$ syntax is used instead of .data$ as they are in different df
  mutated_data <- mutated_data |>
    dplyr::inner_join(configured_data$treatments, by = dplyr::join_by(x$T == y$Number))

  # sort by treatment and calculate y positions
  df <- mutated_data |>
    dplyr::arrange(dplyr::desc(.data$Label), .data$Study)
  df$y_position <- baseline_summary_y_position(df$Label)

  # find pretty xmin and xmax
  x_min <- min(df$baseline - df$baseline_error, na.rm = TRUE)
  x_max <- max(df$baseline + df$baseline_error, na.rm = TRUE)

  x_min <- format_xlim(x_min, "min", FALSE)
  x_max <- format_xlim(x_max, "max", FALSE)

  x_ticks <- .FindXTicks(lower = x_min, upper = x_max)

  inches_to_lines <- par("cin")[2]

  # process df data needed for layout
  df$point_estimate <- df$baseline
  df$ci_lower <- df$baseline - df$baseline_error
  df$ci_upper <- df$baseline + df$baseline_error
  if (configured_data$outcome_measure == "OR"){
    df$point_estimate <- stats::plogis(df$point_estimate)
    df$ci_lower <- stats::plogis(df$ci_lower)
    df$ci_upper <- stats::plogis(df$ci_upper)
  }
  df$point_estimate <- df$point_estimate |> sprintf(fmt = "%-.2f")
  df$ci_lower <- df$ci_lower |> sprintf(fmt = "%-.2f")
  df$ci_upper <- df$ci_upper |> sprintf(fmt = "%-.2f")
  df$outcome <- paste0(df$point_estimate, " (", df$ci_lower, ", ", df$ci_upper,  ")")

  # find the longest outcome label
  outcome_width_inches <- max(strwidth(df$outcome, units = "in"))
  outcome_width <- outcome_width_inches / inches_to_lines

  # find the longest label in lines
  longest_treatment_label_inches <- max(strwidth(df$Label, units = "in"))
  longest_study_label_inches <- max(strwidth(df$Study, units = "in")) + inches_to_lines # + 1 for offsetting
  label_width_inches <- max(longest_treatment_label_inches, longest_study_label_inches)
  label_width <- label_width_inches / inches_to_lines

  # set plot dimensions and margins
  top_line <- max(df$y_position)

  bottom_margin <- 12
  top_margin <- 4
  left_margin <- 4 + (outcome_width_inches + label_width_inches + (2 * inches_to_lines)) / inches_to_lines
  right_margin <- 4

  plot_area_width <- 6

  plot_height <- (bottom_margin + top_margin + top_line) * inches_to_lines

  plot_width <- outcome_width_inches +
    label_width_inches +
    (2 * inches_to_lines) +
    plot_area_width

  svg <- svglite::xmlSVG({
    par(family = "Arimo")
    par(mar = c(bottom_margin, left_margin, top_margin, right_margin))

    plot(
      x = NA,
      y = NA,
      xlim = c(x_min, x_max),
      ylim = c(0, top_line + 2),
      yaxt = "n",
      xaxt = "n",
      ylab = "",
      xlab = "Baseline risk",
      frame.plot = F, # remove border
      yaxs = "i" # remove internal padding
    )

    title("Baseline risk")

    # define the ticks and labels for the x-axis
    if (is.element(configured_data$outcome_measure, c("OR", "RR"))) {
      x_labels <- signif(stats::plogis(x_ticks), digits = 2)
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

    # add the mean line
    abline(v = mean(df$baseline, na.rm = TRUE), lty = 2)

    # add header of the within-study comparison-level outcomes
    mtext(
      text = c("Baseline risk", "(95% CI)"),
      side = 2,
      at = c(top_line + 3.2, top_line + 2),
      line = 1 + (outcome_width * 0.5), # centered
      adj = 0.5,
      las = 1,
      font = 2
    )

    treats <- unique(df$Label)
    n_treats <- length(treats)

    for (row in 1:n_treats) {
      baseline_summary_block(
        df = df,
        label_x_position = label_x_position,
        outcome_x_position = outcome_x_position,
        y_positions = df$y_position,
        treatment = treats[row]
      )
    }

    # legend
    mtext(
      PasteCaptionText("baseline risk", error_bar_text),
      side = 2,
      line = label_x_position - 1,
      las = 1,
      at = -6:-9,
      adj = 0
    )

  },
  width = plot_width,
  height = plot_height,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)
}

#' Paste the caption text together
#'
#' @param caption_setting Text string to describe type of plot. Can be "baseline risk" or "covariate"
#' @param error_bar_text Text string to explain the error bar (optional)
#' @return Text string to be used for caption
#' @noRd
PasteCaptionText <- function(caption_setting, error_bar_text = NULL) {

  caption_text <- paste('The plotted', caption_setting, 'value is the same for all treatment arms across')

  if(caption_setting == "baseline risk") {
    caption_text <- c(caption_text,
                      "a study and is the outcome in the reference treatment arm.",
                      "Values for studies without a reference treatment arm are not plotted.",
                      error_bar_text)
  }

  return(caption_text)

}

#' Convert a vector of treatments to y positions by adding one
#' when the previous treatment is identical, or three if it is different
#'
#' @param treatments vector of treatments
#' @return vector of y positions
#' @noRd
baseline_summary_y_position <- function(treatments){
  result <- numeric(length(treatments))
  result[1] <- 1  # Start with 1 for the first element

  for(i in 2:length(treatments)) {
    if(treatments[i] == treatments[i-1]) {
      result[i] <- result[i-1] + 1
    } else {
      result[i] <- result[i-1] + 3
    }
  }
  result
}

#' Add the treatment effect line and confidence interval for a single comparison in a single study.
#'
#' @param df Dataframe of the data to plot
#' @param y_position y co-ordinate for the study label and confidence interval.
#' @param studlab Study.
#' @param treatment Treatment
#' @param ci_limit_height Height of the vertical lines at the end of the confidence intervals. Defaults to 0.3.
#' @importFrom graphics points lines
#' @noRd
baseline_summary_treatment_effect <- function(df, y_position, studlab, treatment, ci_limit_height = 0.3) {
  row <- which(df$Study == studlab & df$Label == treatment)
  # point estimate
  points(
    x = df$baseline[row],
    y = y_position,
    pch = 18
  )

  # horizontal line
  lines(
    x = c(df$baseline[row] - df$baseline_error[row], df$baseline[row] + df$baseline_error[row]),
    y = rep(y_position, times = 2)
  )
  # vertical lines
  lines(
    x = rep(df$baseline[row] - df$baseline_error[row], times = 2),
    y = c(y_position - ci_limit_height, y_position + ci_limit_height)
  )
  lines(
    x = rep(df$baseline[row] + df$baseline_error[row], times = 2),
    y = c(y_position - ci_limit_height, y_position + ci_limit_height)
  )


}

#' Add the effects size and confidence interval text for a single comparison in a single study.
#'
#' @param df Dataframe of the data to plot
#' @param x_position x co-ordinate for the start of the text.
#' @param y_position y co-ordinate for the effect size and confidence interval.
#' @param studlab Study.
#' @param treatment Treatment.
#' @noRd
baseline_summary_outcome_text <- function(df, x_position, studlab, treatment) {
  row <- which(df$Study == studlab & df$Label == treatment)
  # exclude if missing
  if (!grepl("NA", df$outcome[row])){
    graphics::mtext(
      text = df$outcome[row],
      side = 2,
      line = x_position,
      at = df$y_position[row],
      las = 1,
      adj = 0
    )
  }
}

#' Add the treatment effect lines, confidence intervals, studies, and comparison header, for a single comparison.
#'
#' @param df Dataframe of the data to plot
#' @param label_x_position  x co-ordinate for the start of the labels.
#' @param outcome_x_position x co-ordinate for the start of the text.
#' @param y_positions Vector of y co-ordinates for the study labels and confidence intervals.
#' @param treatment Treatment.
#' @noRd
baseline_summary_block <- function(df, label_x_position, outcome_x_position, y_positions, treatment) {

  # select the rows comparing to the two treatments
  df_subset <- df[df$Label == treatment,]

  y_header_position <- max(df_subset$y_position) + 1

  # header
  graphics::mtext(
    text = treatment,
    side = 2,
    line = label_x_position,
    at = y_header_position + 0.2,
    las = 1,
    adj = 0,
    font = 2
  )

  # study labels
  graphics::mtext(
    text = df_subset$Study,
    side = 2,
    line = label_x_position - 1,
    at = df_subset$y_position,
    las = 1,
    adj = 0
  )

  for (row in 1:length(df_subset$Label)) {
    baseline_summary_treatment_effect(
      df = df_subset,
      y_position = df_subset$y_position[row],
      studlab = df_subset$Study[row],
      treatment = df_subset$Label[row]
    )
    baseline_summary_outcome_text(
      df = df_subset,
      x_position = outcome_x_position,
      studlab = df_subset$Study[row],
      treatment = df_subset$Label[row]
    )
  }
}
