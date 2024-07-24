
#' Module UI for the nodesplit model page
#' 
#' @param id ID of the module
#' @return Div for the panel
nodesplit_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    p(
      "Please note: This may take more than 10 minutes depending on the number of treatment options. The node splitting option for
      the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  We have produced a",
      tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Local-User-Guide", "guide",target="_blank"),
      "to running MetaInsight locally through RStudio on the user's own machine if they want to make use of this function."
    ),
    fluidRow(
      column(
        width = 6,
        p(tags$strong("Inconsistency test with notesplitting model for all studies")),
        nodesplit_panel_ui(id = ns("all"), item_name = "all studies")
      ),
      column(
        width = 6,
        p(tags$strong("Inconsistency test with notesplitting model with studies excluded")),
        nodesplit_panel_ui(id = ns("sub"), item_name = "the sensitivity analysis")
      )
    )
  )
}


#' Module server for the nodesplit model page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
nodesplit_page_server <- function(
    id,
    data,
    sensitivity_data,
    treatment_df,
    sensitivity_treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects
    ) {
  moduleServer(id, function(input, output, session) {
    nodesplit_panel_server(
      id = "all",
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects
    )
    
    nodesplit_panel_server(
      id = "sub",
      data = sensitivity_data,
      treatment_df = sensitivity_treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects
    )
  })
}