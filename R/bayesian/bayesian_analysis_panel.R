
#' Module UI for the bayesian analysis panel
#' 
#' @param id ID of the module
#' @return Div for the panel
bayesian_analysis_panel_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  ui = div(
    tabsetPanel(
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Forest plot"),
        bayesian_forest_plots_page_ui(id = ns("forest_plots"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Comparison of all treatment pairs"),
        bayesian_treatment_comparisons_page_ui(id = ns("treatment_comparisons"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Ranking"),
        ranking_page_ui(id = ns("ranking"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Nodesplit model"),
        nodesplit_page_ui(id = ns("nodesplit"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Bayesian result details"),
        result_details_page_ui(id = ns("result_details"), item_names = c("all studies", "the sensitivity analysis"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Deviance report"),
        deviance_report_page_ui(id = ns("deviance_report"), item_names = c("all studies", "the sensitivity analysis"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Model details"),
        model_details_panel_ui(id = ns("model_details"), c("all studies", " the sensitivity analysis"), page_numbering)
      )
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}


#' Module server for the bayesian analysis panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param sensitivity_data Reactive containing data to analyse for sensitivity analysis
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param sensitivity_treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label) for sensitivity analysis
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param continuous_outcome Reactive containing acronym of the continuous outcome:
#'   "MD" for mean difference, or "SMD" for standardised mean difference
#' @param binary_outcome Reactive containing acronym of the binary outcome:
#'   "OR" for odds ratio, "RR" for risk ratio, or "RD" for risk difference
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param bugsnetdt_sub Reactive containing bugsnet meta-analysis for sensitivity analysis
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
bayesian_analysis_panel_server <- function(
    id,
    data,
    sensitivity_data,
    treatment_df,
    sensitivity_treatment_df,
    metaoutcome,
    outcome_measure,
    continuous_outcome,
    binary_outcome,
    model_effects,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt,
    bugsnetdt_sub,
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
      sensitivity_data = sensitivity_data,
      treatment_df = treatment_df,
      sensitivity_treatment_df = sensitivity_treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      bugsnetdt = bugsnetdt,
      bugsnetdt_sub = bugsnetdt_sub,
      reference_alter = reference_alter
    )
    
    model <- forest_plots_reactives$model
    model_sub <- forest_plots_reactives$model_sub
    model_valid <- forest_plots_reactives$model_valid
    model_sub_valid <- forest_plots_reactives$model_sub_valid

    # 3b. Comparison of all treatment pairs
    bayesian_treatment_comparisons_page_server(
      id = "treatment_comparisons",
      model = model,
      model_sub = model_sub,
      outcome_measure = outcome_measure,
      model_valid = model_valid,
      model_sub_valid = model_sub_valid
    )

    # 3c. Ranking Panel
    ranking_page_server(
      id = "ranking",
      model = model,
      model_sub = model_sub,
      data = data,
      sensitivity_data = sensitivity_data,
      treatment_df = treatment_df,
      sensitivity_treatment_df = sensitivity_treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt,
      bugsnetdt_sub = bugsnetdt_sub
    )

    # 3d. Nodesplit model
    nodesplit_page_server(
      id = "nodesplit",
      data = data,
      sensitivity_data = sensitivity_data,
      treatment_df = treatment_df,
      sensitivity_treatment_df = sensitivity_treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects
    )

    # 3e. Bayesian result details
    result_details_page_server(
      id = "result_details",
      models = c(model, model_sub)
    )

    # 3f. Deviance report
    deviance_report_page_server(
      id = "deviance_report",
      models = c(model, model_sub)
    )

    # 3g. Model details
    model_details_panel_server(
      id = "model_details",
      models = c(model, model_sub)
    )
  })
}