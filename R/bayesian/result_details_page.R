
#' Module UI for the result details page
#' 
#' @param id ID of the module
#' @return Div for the panel
result_details_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    fluidRow(
      column(
        width = 6,
        result_details_panel_ui(id = ns("gemtc_results"))
      ),
      column(
        width = 6,
        result_details_panel_ui(id = ns("gemtc_results_sub"))
      )
    )
  )
}


#' Module server for the result details page.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
result_details_page_server <- function(id, model, model_sub) {
  moduleServer(id, function(input, output, session) {
    result_details_panel_server(
      id = "gemtc_results",
      summary = reactive({ model()$sumresults }),
      samples = reactive ({ model()$mtcResults })
    )
    result_details_panel_server(
      id = "gemtc_results_sub",
      summary = reactive({ model_sub()$sumresults }),
      samples = reactive ({ model_sub()$mtcResults })
    )
  })
}