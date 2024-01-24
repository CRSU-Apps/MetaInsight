
#' Module UI for the result details page
#' 
#' @param id ID of the module
#' @return Div for the panel
regression_result_details_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    fixedRow(
      result_details_panel_ui(id = ns("gemtc_results"))
    )
  )
}


#' Module server for the result details page.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
regression_result_details_page_server <- function(id, model) {
  moduleServer(id, function(input, output, session) {
    result_details_panel_server(
      id = "gemtc_results",
      summary = reactive({ summary(model()) }),
      samples = reactive({ model()$samples })
    )
  })
}