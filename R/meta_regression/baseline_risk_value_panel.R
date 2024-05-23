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
      h4("Covariate value:", textOutput(ns("mean_covariate_value"), inline = TRUE), tags$i(class="fa-regular fa-circle-question")),
      style = "padding-right: 5pt;"
    )
  )
}

#' Create the covariate value panel server.
#'
#' @param id ID of the module.
#' @param reference_outcomes Reactive containing the observed outcomes in the reference arm.
baseline_risk_value_panel_server <- function(id, reference_outcomes) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$mean_covariate_value <- renderText(
      round(mean(reference_outcomes, na.rm = TRUE),
            digits = 3)
      )
    outputOptions(x = output, name = "mean_covariate_value", suspendWhenHidden = FALSE)
    
  })
}
