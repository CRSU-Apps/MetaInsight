#' Module UI for the covariate summary plot tab.
#' 
#' @param id ID of the module
#' @return Tab 4a Summary
metaregression_summary_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Summary Characteristic Plot"),
    uiOutput(ns("toggle_covariate_baseline")),
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
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @return Covariate summary plot tab
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
    
    # Render covariate summary plot
    observe({
      output$covariate_plot <- renderPlot({
  
        CreateCovariateSummaryPlot(all_data(), metaoutcome(), input$toggle_covariate_baseline)
        
      }, width = calculate_plot_pixel(nrow(all_data()))
      )
    })
  
    output$test <- renderPrint ({ all_data() })
    

    # Not working
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
