#' Module UI for the covariate summary plot tab.
#' 
#' @param id ID of the module
#' @return Tab 4a Summary
metaregression_summary_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4('Summary Characteristic Plot'),
    uiOutput(ns('toggle')),
    plotOutput(outputId = ns('covariate_plot'), height = "auto"), 
    # height = "auto" prevents download button overlapping for longer plots
    textOutput(outputId = ns('covariate_info')),
    radioButtons(inputId = ns('format_covariate_plot'),
                 label = 'Document format',
                 choices = c('PDF' = 'pdf', 'PNG' = 'png'),
                 inline = TRUE),
    downloadButton(outputId = ns('download_covariate_summary')),
  )
}

#' Module server for the covariate summary plot tab.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @param metaoutcome Reactive containing meta analysis outcome: 'Continuous' or 'Binary'
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' 
metaregression_summary_panel_server <- function(id, all_data, metaoutcome, treatment_df) {
  moduleServer(id, function(input, output, session) {
    
    # Toggle between covariate and baseline risk, only when there is a covariate
    observe({
      output$toggle <- renderUI({
        # If there is a covariate (i.e. the covariate name is not NA)
        if(!is.na(FindCovariateNames(all_data())[1])) {
          shinyWidgets::radioGroupButtons(
                inputId = session$ns('toggle_covariate_baseline'),
                choices = c('Covariate', 'Baseline risk'),
                status = 'primary'
          )
        }
      })
    })
    
    # Convert wide data to long format as needed for CreateCovariateSummaryPlot
    long_data <- reactive({
      if (FindDataShape(all_data()) == "wide") {
        return(WideToLong(all_data(), metaoutcome()))
      } else {
        return(all_data())
      }
    })
    
    # Render covariate summary plot
    # Needs observe wrapper due to height argument
    observe({
      output$covariate_plot <- renderPlot({
  
        CreateCovariateSummaryPlot(long_data(), 
                                   metaoutcome(), 
                                   # Prevents momentary error in app when plotting
                                   ifelse(is.null(input$toggle_covariate_baseline), 'Baseline risk', input$toggle_covariate_baseline), 
                                   treatment_df()
                                   )
        
  
      
      }, height = calculate_plot_pixel(nrow(long_data()))
      )
    })

    # Covariate summary plot download handler
    output$download_covariate_summary <- downloadHandler(
      filename = function() {
        paste0('4a_Summary.', input$format_covariate_plot)
      },
      content = function(file) {
        
        ggsave(filename = file, 
               device = input$format_covariate_plot,
               plot = CreateCovariateSummaryPlot(long_data(), 
                                                 metaoutcome(), 
                                                 ifelse(is.null(input$toggle_covariate_baseline), 'Baseline risk', input$toggle_covariate_baseline), 
                                                 treatment_df()
               ),
               height = PlotDownloadHeight(nrow(long_data())),
               units = "px"
        )
      }
    )
  })
}
