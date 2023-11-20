#' Module UI for the covariate summary plot tab.
#' 
#' @param id ID of the module
#' @return Tab 4a Summary
metaregression_summary_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("toggle_covariate_baseline")),
    plotOutput(outputId = ns('covariate_plot')),
    p('The covariate value is the same for all treatment arms across a study.'),
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
              inputId = session$ns("covariate_baseline_toggle"),
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
      
      # BUGSnet data prep to convert all_data to format required for data.plot
      BUGSnet_data <- BUGSnet::data.prep(arm.data = all_data(),
                                         varname.t = 'T',
                                         varname.s = 'Study')
      
      # If there is a covariate (i.e. the covariate name is not NA)
      if (!is.na(FindCovariateNames(all_data())[1])) {
        
        # Use FindCovariateName function to find the covariate column
        covariate <- FindCovariateNames(all_data())[1]
        
        # Use GetFriendlyCovariateName function to label the y-axis
        y_axis_label <- GetFriendlyCovariateName(covariate)
        
      }
      
      # # Baseline risk for continuous outcomes
      # else if (metaoutcome() == "Continuous") {
      #   # The covariate is the mean column for the reference treatment
      #   covariate <- ?
      #   y_axis_label <- "Baseline risk"
      # }

      return(DataPlot(BUGSnet_data,
                                 covariate = covariate,
                                 covariate.label = y_axis_label,
                                 # half.length = "age_SD", # Error bars - needs a second covariate, possible future addition
                                 by = 'treatment',
                                 text.size = 16) 
             )
    }
    
    # Render covariate summary plot
    output$covariate_plot <- renderPlot({

      make_covariate_plot(all_data)
      
    })
    
    # output$test <- renderPrint ({ FindCovariateNames(all_data())[1] })
    output$test <- renderPrint ({(!is.na(FindCovariateNames(all_data())[1])) })
    

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
