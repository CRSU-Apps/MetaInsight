
#' Module UI for the diagnostics page
#' 
#' @param id ID of the module
#' @param item_names Vector of item names to be shown side-by-side in the page.
#' @return Div for the panel
diagnostics_page_ui <- function(id, item_names, page_numbering) {
  ns <- NS(id)

  page_numbering$DiveLevel()
  
  ui = tabsetPanel(
    tabPanel(
      title = paste0(page_numbering$AddChild(), " MCMC"),
      mcmc_page_ui(id = ns("mcmc"), item_names = item_names)
    ),
    tabPanel(
      title = paste0(page_numbering$AddChild(), " Deviance"),
      deviance_report_page_ui(id = ns("deviance"), item_names = item_names)
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}


#' Module server for the diagnostics page.
#' 
#' @param id ID of the module.
#' @param models Vector of reactives containing bayesian meta-analyses.
#' @param models_valid Vector of reactives containing whether each model is valid.
#' @param package "gemtc" (default) or "bnma".
diagnostics_page_server <- function(id, models, models_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    mcmc_page_server(id = "mcmc", models = models, models_valid = models_valid, package = package)
    deviance_report_page_server(id = "deviance", models = models, models_valid = models_valid, package = package)
  })
}

