#' Create the covariate summary plot 
#' https://rdrr.io/github/audrey-b/BUGSnet/man/data.plot.html
#'
#' @param all_data Study data including covariate columns, in wide or long format
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param covariate_or_baseline Output from widget to toggle between covariate and baseline risk for the plot type.
#' @return BUGSnet::data.plot plot

CreateCovariateSummaryPlot <- function(all_data, metaoutcome, covariate_or_baseline) {

  # If there is no covariate, or baseline risk is selected by the toggle
  if (is.na(FindCovariateNames(all_data)[1]) || covariate_or_baseline == "Baseline risk") {
    
    y_axis_label <- "Baseline risk"
    caption_setting <- "baseline risk" # Text to add to caption
    covariate <- "baseline" # Column of df selected for plot
    
    # Baseline risk for continuous outcomes
    if (metaoutcome == "Continuous") {
      
      # Add baseline column that is the mean value and 
      # baseline_error column that is 1.96 * SD / sqrt(N)
      # of the reference arm for the study, or NA if there is no reference arm
      mutated_data <- all_data %>% 
        dplyr::group_by(Study) %>%
        dplyr::mutate(
          # Reference arm is always numbered 1 internally
          baseline = ifelse(is.null(Mean[T == 1]), NA, Mean[T == 1])
        ) %>%
        dplyr::mutate(
          baseline_error = ifelse(is.null(Mean[T == 1]), NA, (1.96 * SD[T == 1]) / sqrt(N[T == 1]))
        )
      
      # Error bar text for plot caption
      error_bar_text <- "Error bars: mean +/- 1.96 * SD / sqrt(N)"
      
    # Baseline risk for binary outcomes
    } else if (metaoutcome == "Binary") {

      # Use escalc function with the logit transformed proportion (PLO) measure
      effects <- metafor::escalc(measure="PLO", xi = all_data$R, ni = all_data$N)
      
      # Merge effects and all_data into one df
      mutated_data <- cbind(all_data, effects)
      
      # Add baseline column that is yi column from escalc
      # baseline_error column that is 1.96 * sqrt(vi) column from escalc
      # of the reference arm for the study, or NA if there is no reference arm
      mutated_data <- mutated_data %>%
        dplyr::group_by(Study) %>%
        dplyr::mutate(
          # Reference arm is always numbered 1 internally
          baseline = ifelse(is.null(R[T == 1]), NA, yi[T == 1])
        ) %>%
        dplyr::mutate(
          baseline_error = ifelse(is.null(R[T == 1]), NA, 1.96 * sqrt(vi[T == 1]))
        )
      
      # Error bar text for plot caption
      error_bar_text <- "Error bars: effect size +/- 1.96 * sqrt(variance)"
     
    # Error message
    } else {
      stop("The outcome should be either continuous or binary")
    }
    
    # Convert tibble created by dplyr to df
    BUGSnet_df <- as.data.frame(mutated_data)
    
  }
  
  # If there is a covariate 
  else {
    
    # Use FindCovariateName function to find the covariate column
    covariate <- FindCovariateNames(all_data)[1]
    
    # Use GetFriendlyCovariateName function to label the y-axis
    y_axis_label <- GetFriendlyCovariateName(covariate)
    
    caption_setting <- "covariate" # Text to add to caption
    BUGSnet_df <- all_data # Rename for input to BUGSnet::data.prep
    
  } 
    
  # BUGSnet data prep to convert data to format required for data.plot
  BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet_df,
                                     varname.t = 'T',
                                     varname.s = 'Study')
  
  # Baseline risk plot with error bars
  if (caption_setting == "baseline risk") {
    
    plot <- BUGSnet::data.plot(BUGSnet_data,
                               covariate = covariate,
                               covariate.label = y_axis_label,
                               half.length = "baseline_error",
                               by = 'treatment',
                               text.size = 16)
    
    # Add caption text under plot
    plot <- plot +
      labs(caption = PasteCaptionText(caption_setting, error_bar_text))
    
    if (metaoutcome == "Binary") {
      
      # Plot in logit scale, label on probability scale
      plot <- plot +
        scale_y_continuous(labels = function(x) round(100 * plogis(x), digits = 1))
      
    }
    
  # Covariate plot without error bars
  } else {
    
    plot <- BUGSnet::data.plot(BUGSnet_data,
                               covariate = covariate,
                               covariate.label = y_axis_label,
                               by = 'treatment',
                               text.size = 16) 
    
    # Add caption text under plot
    plot <- plot +
      labs(caption = PasteCaptionText(caption_setting))
  }
  
  plot <- plot +
    # Right aligned caption text
    theme(plot.caption = element_text(hjust = 1)) 

  
  return(plot)
  
}

#' Paste the caption text
#'
#' @param plot_type Text string to describe type of plot. Can be "baseline risk" or "covariate"
#' @param error_bar_text Text string to explain the error bar (optional)
#' @return Text string to be used for caption

PasteCaptionText <- function(caption_setting, error_bar_text = NULL) {
  
  # Short lines because the line lengths are not reactive to plot width and I haven't found a fix
  caption_text <- paste('The plotted', caption_setting, 'value is the same for all treatment arms across a study 
                              and represents the', caption_setting, 'value in the reference treatment arm.
                              Values for studies without a reference treatment arm are not plotted. \n', 
                              error_bar_text)
  
  return(caption_text)
  
}
  

