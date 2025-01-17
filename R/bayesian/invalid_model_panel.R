
#' Module UI for the invalid model panel.
#' 
#' @param id ID of the module.
#' @return uiOutput for the panel.
invalid_model_panel_ui <- function(id) {
  ns <- NS(id)
  return(uiOutput(outputId = ns("invalid_model")))
}


#' Module server for the nodesplit model panel.
#' 
#' @param id ID of the module
#' @param model_valid Reactive containing whether the model is valid.
invalid_model_panel_server <- function(id, model_valid) {
  moduleServer(
    id,
    function(input, output, session) {
      output$invalid_model <- renderUI({
        if (model_valid()) {
          return()
        } else {
          return(
            div(
              h3("Please rerun model"),
              style = ""
            )
          )
        }
      })
    }
  )
}
