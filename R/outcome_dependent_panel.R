#' Create the meta-regression panel.
#'
#' @param id ID of the module
#' @param inner_ui_expression Expression for the UI function for this item.
#' 
#' @return Div containing the module
outcome_dependent_ui <- function(id, inner_ui_expression) {
  ns <- NS(id)
  div(
    conditionalPanel(
      condition = "!output.supported",
      ns = ns,
      div(
        h3(
          textOutput(outputId = ns("error_message"))
        ),
        style = "color: red; font-style: italic; font-weight: bold;"
      )
    ),
    conditionalPanel(
      condition = "output.supported",
      ns = ns,
      eval(
        expr = substitute(inner_ui_expression)
      )
    )
  )
}

#' Create the outcome-dependent server. This will hide the given UI and display an
#' error message when the outcome measure is not supported.
#'
#' @param id ID of the module
#' @param outcome_measure Reactive containing the outcome measure of the analysis.
#' @param supported_measures Vector of outcome measures which are supported for this item.
#' @param inner_server_expression Expression for the server function for this item.
outcome_dependent_server <- function(id, outcome_measure, supported_measures, inner_server_expression) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$error_message <- renderText({
      paste0(
        "Outcome measure is not supported for this analysis. Supported types are: ",
        paste0(supported_measures, collapse = ", ")
      )
    })
    
    output$supported <- reactive({
      outcome_measure() %in% supported_measures
    })
    shiny::outputOptions(x = output, name = "supported", suspendWhenHidden = FALSE)
    
    eval(
      expr = substitute(inner_server_expression)
    )
  })
}
