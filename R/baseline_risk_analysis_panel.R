#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      tabPanel(
        title = "4b-1. Regression plot",
        covariate_run_model_ui(id = ns("baseline_risk_model"))
      ),
      tabPanel(
        title = "4b-2. Forest plot",
        fixedRow(
          align = "center",
          uiOutput(outputId = ns("convergence_warning")),
          bayesian_forest_plot_plus_stats_ui(id = ns("baseline_risk_forest"))
        )
      ),
      tabPanel(
        title = "4b-3. Comparison of all treatment pairs",
        covariate_treatment_comparisons_page_ui(id = ns("baseline_risk_treatment_comparisons"))
      ),
      tabPanel(
        title = "4b-4. Ranking",
        covariate_ranking_page_ui(id = ns("baseline_risk_ranking"))
      )
    )
  )
}






#' Create the baseline risk analysis server.
#'
#' @param id ID of the module
baseline_risk_analysis_panel_server <- function(    id, 
                                                    all_data,
                                                    treatment_df,
                                                    reference_treatment,
                                                    metaoutcome,
                                                    outcome_measure,
                                                    model_effects,
                                                    bugsnetdt
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    model_reactive <- baseline_risk_run_model_server(
      id = "baseline_risk_model",
      data = all_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      model_effects = model_effects
    )
    
    #Outcome in the reference arm, needed for the graph
    covariate_data <- reactive({GetReferenceOutcome(data = all_data(),
                                                    treatments = treatment_df()$Label[order(treatment_df()$Number)],
                                                    outcome_type = metaoutcome())
      })
    
    #Warn if the model did not converge
    output$convergence_warning <- renderText({
        ifelse(model_reactive()$max.gelman > 1.05,
               "<b><h3> Warning: Model did not converge according to Gelman-Rubin diagnostics </h3></b>",
               "")
    })
    
    # 4b-2 Create forest plot and associated statistics
    bayesian_forest_plot_plus_stats_baseline_risk_server(
      id = "baseline_risk_forest",
      model_reactive = model_reactive,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt
    )
    
    # 4b-3 Treatment comparisons
    baseline_risk_treatment_comparisons_page_server(
      id = "baseline_risk_treatment_comparisons",
      model = model_reactive,
      outcome_measure = outcome_measure
    )
    
    # 4c-4 Ranking Panel
    covariate_ranking_page_server(
      id = "cov_ranking",
      model = model_output,
      data = all_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      rank_option = rank_option,
      freq_all = freq_all,
      bugsnetdt = bugsnetdt,
      cov_value = covariate_value
    )
      
    })
}

