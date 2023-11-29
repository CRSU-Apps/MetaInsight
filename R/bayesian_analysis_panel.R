
#' Module UI for the bayesian analysis panel
#' 
#' @param id ID of the module
#' @return Div for the panel
bayesian_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    tabsetPanel(
      tabPanel(
        title = "3a. Forest plot",
        bayesian_forest_plots_page_ui(id = ns("forest_plots"))
      ),
      tabPanel(
        title = "3b. Comparison of all treatment pairs",
        bayesian_treatment_comparisons_page_ui(id = ns("treatment_comparisons"))
      ),
      tabPanel(
        title = "3c. Ranking",
        ranking_page_ui(id = ns("ranking"))
      ),
      tabPanel(
        title = "3d. Nodesplit model",
        nodesplit_panel_ui(id = ns("nodesplit"))
      ),
      tabPanel(
        title = "3e. Bayesian result details",
        result_details_panel_ui(id = ns("result_details"))
      ),
      tabPanel(
        title = "3f. Deviance report",
        deviance_report_panel_ui(id = ns("deviance_report"))
      ),
      tabPanel(
        title = "3g. Model details",
        model_details_panel_ui(id = ns("model_details"))
      )
    )
  )
}


#' Module server for the bayesian analysis panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param continuous_outcome Reactive containing acronym of the continuous outcome:
#'   "MD" for mean difference, or "SMD" for standardised mean difference
#' @param binary_outcome Reactive containing acronym of the binary outcome:
#'   "OR" for odds ratio, "RR" for risk ratio, or "RD" for risk difference
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
bayesian_analysis_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    continuous_outcome,
    binary_outcome,
    model_effects,
    exclusions,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt,
    reference_alter
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### SMD warning alert

    observeEvent(list(input$node, input$node_sub), {
      if (continuous_outcome()=="SMD") {
        showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
      else if (binary_outcome()=="RD") {
        showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
    })
    
    # 3a. Forest plots
    forest_plots_reactives <- bayesian_forest_plots_page_server(
      id = "forest_plots",
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      continuous_outcome = continuous_outcome,
      binary_outcome = binary_outcome,
      model_effects = model_effects,
      exclusions = exclusions,
      bugsnetdt = bugsnetdt,
      reference_alter = reference_alter
    )
    
    model <- forest_plots_reactives$model
    model_sub <- forest_plots_reactives$model_sub


    # 3b. Comparison of all treatment pairs
    bayesian_treatment_comparisons_page_server(
      id = "treatment_comparisons",
      model = model,
      model_sub = model_sub,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure
    )

    # 3c. Ranking Panel
    ranking_page_server(
      id = "ranking",
      model = model,
      model_sub = model_sub,
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      exclusions = exclusions,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt
    )

    # 3d. Nodesplit model
    nodesplit_panel_server(
      id = "nodesplit",
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      exclusions = exclusions
    )

    # 3e. Bayesian result details
    result_details_panel_server(
      id = "result_details",
      model = model,
      model_sub = model_sub
    )

    # 3f. Deviance report
    deviance_report_panel_server(
      id = "deviance_report",
      model = model,
      model_sub = model_sub
    )

    # 3g. Model details
    model_details_panel_server(
      id = "model_details",
      model = model,
      model_sub = model_sub
    )
  })
}