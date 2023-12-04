
#' Module UI for the ranking page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
ranking_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page.",
      tags$br(),
      tags$strong("Please note it may take up to 5 minutes to load the results.", style = "color:#FF0000"),
      tags$br(),
      tags$strong(
        "IMPORTANT: If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:",
        style = "color:#4863A0"
      ),
      tags$a(
        href = "https://doi.org/10.1016/j.jclinepi.2023.02.016",
        "Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed to aid interpretation of network meta-analysis,
        Journal of Clinical Epidemiology (2023)"
      )
    ),
    fluidRow(
      ranking_panel_ui(id = ns("rank_all"), title = "Ranking panel for all studies", table_label = "Ranking probabilities and SUCRA values for all treatments")
    ),
    fluidRow(
      ranking_panel_ui(id = ns("rank_sub"), title = "Ranking panel with studies excluded", table_label = "Ranking probabilities and SUCRA values for all treatments")
    )
  )
}


#' Module server for the ranking page.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis for all studies.
#' @param model_sub Reactive containing meta-analysis with studies excluded.
#' @param data Reactive containing data to analyse.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label).
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD".
#' @param model_effects Reactive containing model effects: either "random" or "fixed".
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis.
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not.
#' @param freq_all Reactive containing frequentist meta-analysis.
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis.
#' @param bugsnetdt Reactive containing bugsnet meta-analysis.
ranking_page_server <- function(
    id,
    model,
    model_sub,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    exclusions,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt
    ) {
  
  moduleServer(id, function(input, output, session) {
    ranking_panel_server(
      id = "rank_all",
      data = data,
      treatment_df = treatment_df,
      model = model,
      metaoutcome = metaoutcome,
      rank_option = rank_option,
      frequentist = freq_all,
      bugsnetdt = bugsnetdt,
      filename_prefix = "all_studies_",
      title_prefix = "All Studies"
    )
    
    ranking_panel_server(
      id = "rank_sub",
      data = data,
      treatment_df = treatment_df,
      model = model_sub,
      metaoutcome = metaoutcome,
      rank_option = rank_option,
      frequentist = freq_sub,
      bugsnetdt = bugsnetdt,
      filename_prefix = "filtered_studies_",
      title_prefix = "Filtered Studies",
      exclusions = exclusions
    )
  })
}