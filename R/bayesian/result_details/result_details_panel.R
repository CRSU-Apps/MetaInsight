
#' Module UI for the result details panel
#' 
#' @param id ID of the module
#' @param item_name Name of this deviance report item.
#' @return Div for the panel
result_details_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    invalid_model_panel_ui(id = ns("model_invalid")),
    p(tags$strong(glue::glue("Results details for {item_name}"))),
    shinycssloaders::withSpinner(
      verbatimTextOutput(outputId = ns("gemtc_results")),
      type = 6
    )
  )
}



#' Module server for the result details panel.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis.
#' @param package "gemtc" (default) or "bnma".
#' @param model_valid Reactive containing whether the model is valid.
result_details_panel_server <- function(id, model, model_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    
    # Results details
    output$gemtc_results <- renderPrint({
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(model()$sumresults)
      } else if (package == "bnma") {
        return(summary(model()))
      }
    })

  })
}