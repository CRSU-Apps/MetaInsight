
#' Module UI for the result details panel
#' 
#' @param id ID of the module
#' @param item_name Name of this deviance report item.
#' @return Div for the panel
result_details_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    p(tags$strong(glue::glue("Results details for {item_name}"))),
    verbatimTextOutput(outputId = ns("gemtc_results")),
    p(tags$strong(glue::glue("Gelman convergence assessment plot for {item_name}"))),
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
