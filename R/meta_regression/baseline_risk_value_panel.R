.CreateInlineBlock <- function(..., style = NULL) {
  return(
    div(
      ...,
      style = paste0("display: inline-block; ", style)
    )
  )
}

#' Create the baseline risk value panel.
#'
#' @param id ID of the module.
#' @return Div containing the module UI.
baseline_risk_value_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    title = "This value is used by all baseline risk analysis output",
    style = "padding-left: 5px;",
    .CreateInlineBlock(
      h4("Covariate value:", textOutput(ns("centring_value"), inline = TRUE), tags$i(class="fa-regular fa-circle-question")),
      style = "padding-right: 5pt;"
    )
  )
}

#' Create the covariate value panel server.
#'
#' @param id ID of the module.
#' @param model The model, created by BaselineRiskRegression().
baseline_risk_value_panel_server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$centring_value <- renderText(
      round(model$network$mx_bl, digits = 3)
    )
    outputOptions(x = output, name = "centring_value", suspendWhenHidden = FALSE)
    
  })
}
