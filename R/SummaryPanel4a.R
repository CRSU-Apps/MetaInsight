#' Module UI for the covariate summary plot tab.
#' 
#' @param id ID of the module
#' @return Tab 4a Summary
metaregression_summary_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Summary Characteristic Plot"),
    uiOutput(ns("toggle_covariate_baseline")),
    # div(
    #   plotOutput(outputId = ns('covariate_plot')),
    #   style = "transform: rotate(90deg)"
    # ),
    plotOutput(outputId = ns('covariate_plot')),
    textOutput(outputId = ns('covariate_info')),
    verbatimTextOutput(ns("test")),
    radioButtons(inputId = ns('format_covariate_plot'), 
                 label = 'Document format', 
                 choices = c('PDF', 'PNG'), 
                 inline = TRUE),
    downloadButton(outputId = ns('downloadCovariateSummary'))
  )
}

#' Module server for the covariate summary plot tab.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @return Covariate plot from BUGSnet::data.plot
#' 
metaregression_summary_panel_server <- function(id, all_data, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    
    # Toggle between covariate and baseline risk, only when there is a covariate
    output$toggle_covariate_baseline <- renderUI({
      # If there is a covariate (i.e. the covariate name is not NA)
      if(!is.na(FindCovariateNames(all_data())[1])) {
        shinyWidgets::radioGroupButtons(
              inputId = session$ns("toggle_covariate_baseline"),
              choices = c("Covariate", "Baseline risk"),
              status = "primary"
        )
      }
    })
    
    #' Create the covariate summary plot 
    #' https://rdrr.io/github/audrey-b/BUGSnet/man/data.plot.html
    #'
    #' @param all_data Study data including covariate columns, in wide or long format
    #' @return BUGSnet::data.prep plot
    
    make_covariate_plot <- function(all_data) {
      
      # If there is no covariate or baseline risk is selected by the toggle
      if (is.na(FindCovariateNames(all_data())[1]) || input$toggle_covariate_baseline == "Baseline risk") {
        
        y_axis_label <- "Baseline risk"
        caption_setting <- "baseline risk" # Text to add to caption
        covariate <- "baseline" # Column of df selected for plot
        
        # Baseline risk for continuous outcomes
        if (metaoutcome() == "Continuous") {
           
          # Add a baseline column that is the mean value of the reference arm for the study, 
          # or NA if there is no reference arm
          mutated_data <- all_data() %>% 
            dplyr::group_by(Study) %>%
            dplyr::mutate(
              # Reference treatment is always numbered 1 internally
              baseline = ifelse(is.null(Mean[T == 1]), NA, Mean[T == 1])
            )
          
          # Convert tibble from dplyr to df
          BUGSnet_df <- as.data.frame(mutated_data)
        }
      
        # Baseline risk for binary outcomes
        else if (metaoutcome() == "Binary") {
          # The covariate needs to be calculated for the reference treatment
          covariate <- colnames(all_data())[4]
        }
      }
      
      # If there is a covariate (i.e. the covariate name is not NA)
      else if (!is.na(FindCovariateNames(all_data())[1])) {
        
        # Use FindCovariateName function to find the covariate column
        covariate <- FindCovariateNames(all_data())[1]
        
        # Use GetFriendlyCovariateName function to label the y-axis
        y_axis_label <- GetFriendlyCovariateName(covariate)
        
        caption_setting <- "covariate" # Text to add to caption
      }
      
      # BUGSnet data prep to convert data to format required for data.plot
      BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet_df,
                                         varname.t = 'T',
                                         varname.s = 'Study')

      plot <- BUGSnet::data.plot(BUGSnet_data,
                                 covariate = covariate,
                                 covariate.label = y_axis_label,
                                 # half.length = "age_SD", # Error bars - needs a second covariate, possible future addition
                                 by = 'treatment',
                                 text.size = 16) 
      
      # Add caption text under plot
      plot <- plot + 
        labs(caption = paste('The', caption_setting, 'value is the same for all treatment arms across a study.')) +
        theme(plot.caption = element_text(hjust = 0)) # Left aligned
      
      return(plot)
             
    }
    
    # Render covariate summary plot
    observe({
      output$covariate_plot <- renderPlot({
  
        make_covariate_plot(all_data)
        
      }, width = calculate_plot_pixel(nrow(all_data()))
      )
    })
    
    output$test <- renderPrint ({ all_data() })
    # output$test <- renderPrint ({ (is.na(FindCovariateNames(all_data())[1]) || input$toggle_covariate_baseline == "Baseline risk") })
    

    output$downloadCovariateSummary <- downloadHandler(
      filename = function() {
        paste0('4a_Summary.', input$format_covariate_plot)
      },
      content = function(file) {
        draw_covariate_summary <- function() {

          make_covariate_plot(all_data)

        }
        write_to_pdf_or_png(
          file,
          input$format_covariate_plot,
          draw_covariate_summary
        )
      }
    )
    
  })
}
