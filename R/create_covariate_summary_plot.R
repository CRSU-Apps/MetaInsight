#' Create the covariate summary plot using BUGSnet data.plot
#' https://rdrr.io/github/audrey-b/BUGSnet/man/data.plot.html
#'
#' @param all_data Study data including covariate columns, in wide or long format
#' @param metaoutcome Meta-analysis outcome: "Continuous" or "Binary"
#' @param covariate_or_baseline Output from widget to toggle between covariate and baseline risk for the plot type.
#' @param treatment_df Data frame containing treatment IDs (Number) and names (Label)
#' @return BUGSnet::data.plot plot

CreateCovariateSummaryPlot <- function(all_data, metaoutcome, covariate_or_baseline, treatment_df) {
  
  # Convert wide data to long format as needed for CreateCovariateSummaryPlot
  if (FindDataShape(all_data) == "wide") {
    long_data <- WideToLong(all_data, metaoutcome)
  } else {
    long_data <- all_data
  }

  # Baseline risk version of plot
  if (covariate_or_baseline == "Baseline risk") {
    
    # Input settings for plot
    plot_settings <- CreateCovariateSummaryPlotSettings("baseline", all_data) 
    
    # Mutate data
    mutated_data <- MutateCovariateSummaryData(long_data, "baseline", metaoutcome)
    
    if (metaoutcome == "Continuous") {
        
      # Error bar text for continuous outcomes
      error_bar_text <- "Error bars: mean +/- 1.96 * SD / sqrt(N)"
      
    } else if (metaoutcome == "Binary") {
      
      # Error bar text for binary outcomes
      error_bar_text <- "Error bars: logit(outcome) +/- 1.96 * se(logit(outcome))"
     
    } else {
      stop("The outcome should be either continuous or binary")
    }
  }
  
  # Covariate version of plot 
  else {
    
    # Plot settings for covariate plot
    plot_settings <- CreateCovariateSummaryPlotSettings("covariate", all_data)
    
    mutated_data <- long_data # Rename for input to BUGSnet::data.prep
  } 
  
  # browser()
  # Add column with treatment labels
  mutated_data <- mutated_data %>%
    dplyr::inner_join(treatment_df, by = join_by(T == Number)) 
  
  # Convert tibble created by dplyr to df
  BUGSnet_df <- as.data.frame(mutated_data)
    
  # BUGSnet data prep to convert data to format required for data.plot
  BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet_df,
                                     varname.t = 'Label',
                                     varname.s = 'Study')
  
  # Baseline risk plot with error bars
  if (plot_settings$caption_setting == "baseline risk") {
    
    plot <- BUGSnet::data.plot(BUGSnet_data,
                               covariate = plot_settings$covariate,
                               covariate.label = plot_settings$y_axis_label,
                               half.length = "baseline_error",
                               by = 'treatment',
                               text.size = 16,
                               orientation = "portrait")
    
    # Add caption text under plot
    plot <- plot +
      labs(caption = PasteCaptionText(plot_settings$caption_setting, error_bar_text))
    
    if (metaoutcome == "Binary") {

      # Plot in logit scale, label on probability scale
      plot <- plot +
        scale_x_continuous(labels = function(x) signif(plogis(x), digits = 2))
    }
    
  # Covariate plot without error bars
  } else {
    
    plot <- BUGSnet::data.plot(BUGSnet_data,
                               covariate = plot_settings$covariate,
                               covariate.label = plot_settings$y_axis_label,
                               by = 'treatment',
                               text.size = 16,
                               orientation = "portrait") 
    
    # Add caption text under plot
    plot <- plot +
      labs(caption = PasteCaptionText(plot_settings$caption_setting))
  }
  
  plot <- plot +
    theme(plot.caption = element_text(hjust = 1)) + # Right aligned caption text
    # Centre x-axis text & increase spacing between tick and axis labels
    theme(axis.text.x = element_text(angle = 1, vjust = 0, hjust = 0, margin = margin(b = 10))) 

  return(plot)
  
}

#' Create different settings for plot depending if it is baseline risk or covariate
#'
#' @param plot_type Text string to describe type of plot. Can be "baseline" or "covariate"
#' @param all_data Study data including covariate columns, in wide or long format 
#' @return List of plot settings for y_axis_label, caption_setting and covariate (all strings)
#' 
CreateCovariateSummaryPlotSettings <- function(plot_type, all_data) {
  
  # Settings for baseline risk plot
  if (plot_type == "baseline") {
    y_axis_label <- "Baseline risk"
    caption_setting <- "baseline risk" # Text to add to caption
    covariate <- "baseline" # Column of df selected for plot
    
  } else if (plot_type == "covariate") {
    
    # Text to add to caption
    caption_setting <- "covariate" # Text to add to caption
    
    # Use FindCovariateName function to find the covariate column
    covariate <- FindCovariateNames(all_data)[1]
    
    # Use GetFriendlyCovariateName function to label the y-axis
    y_axis_label <- GetFriendlyCovariateName(covariate)
    
  } else {
    
    stop("plot_type should be baseline or covariate") 
  }
  
  
  
  plot_settings <- list("y_axis_label" = y_axis_label,
                        "caption_setting" = caption_setting,
                        "covariate" = covariate)
  
  return(plot_settings)
}

#' Use dplyr to mutate the data into the correct format for use in the covariate summary plots
#' Columns are created based on the reference treatment arm (where one is present)
#' See continuous and baseline sections of function comments for specifics in each case
#'
#' @param long_data Study data including covariate columns in long format 
#' @param plot_type Text string to describe type of plot. Can be "baseline" or "covariate"
#' @param metaoutcome Meta-analysis outcome: "Continuous" or "Binary"
#' @return Mutated data frame ready for BUGSnet data.prep function

MutateCovariateSummaryData <- function(long_data, plot_type, metaoutcome) {
  
  if (plot_type == "baseline") {
    
    # Baseline risk for continuous outcomes
    if (metaoutcome == "Continuous") {
      
      # Add baseline column that is the mean value and 
      # baseline_error column that is 1.96 * SD / sqrt(N)
      # of the reference arm for the study, or NA if there is no reference arm
      # Reference arm is always numbered 1 internally
      mutated_data <- long_data %>% 
        dplyr::group_by(Study) %>%
        dplyr::mutate(
          baseline = ifelse(is.null(Mean[T == 1]), NA, Mean[T == 1])
        ) %>%
        dplyr::mutate(
          baseline_error = ifelse(is.null(Mean[T == 1]), NA, (1.96 * SD[T == 1]) / sqrt(N[T == 1]))
        )
      
    } else if (metaoutcome == "Binary") {
      
      # Use escalc function with the logit transformed proportion (PLO) measure
      effects <- metafor::escalc(measure="PLO", xi = long_data$R, ni = long_data$N)
      
      # Merge effects and long_data into one df
      mutated_data <- cbind(long_data, effects)
      
      # Add baseline column that is yi column from escalc
      # baseline_error column that is 1.96 * sqrt(vi) column from escalc
      # of the reference arm for the study, or NA if there is no reference arm
      # Reference arm is always numbered 1 internally
      mutated_data <- mutated_data %>%
        dplyr::group_by(Study) %>%
        dplyr::mutate(
          baseline = ifelse(is.null(R[T == 1]), NA, yi[T == 1])
        ) %>%
        dplyr::mutate(
          baseline_error = ifelse(is.null(R[T == 1]), NA, 1.96 * sqrt(vi[T == 1]))
        )
      
    } else {
      stop("metaoutcome should be Continous or Binary")
    }
  }
  
  return(mutated_data)
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
  

