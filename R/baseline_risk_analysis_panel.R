#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    h2("This is a placeholder. Please delete this UI element when implementing the real functionality")
  )
}

#' Create the baseline risk analysis server.
#'
#' @param id ID of the module
baseline_risk_analysis_panel_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
