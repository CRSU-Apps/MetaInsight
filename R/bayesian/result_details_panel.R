
#' Module UI for the result details panel
#' 
#' @param id ID of the module
#' @return Div for the panel
result_details_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    p(tags$strong("Results details for all studies")),
    verbatimTextOutput(outputId = ns("gemtc_results")),
    p(tags$strong("Gelman convergence assessment plot for all studies")),
    plotOutput(outputId = ns("gemtc_gelman"))
  )
}


#' Module server for the result details panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis
result_details_panel_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {
    
    # Results details
    output$gemtc_results <- renderPrint ({
      model()$sumresults
    })
    
    # Gelman plots
    output$gemtc_gelman <- renderPlot ({
      gelman.plot(model()$mtcResults)
    })
    
  })
}