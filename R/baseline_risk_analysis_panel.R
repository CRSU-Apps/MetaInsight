#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  ui = fluidPage(
    tabsetPanel(
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Regression plot"),
        covariate_run_model_ui(id = ns("baseline_risk_model"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Forest plot"),
        fixedRow(
          align = "center",
          uiOutput(outputId = ns("convergence_warning")),
          bayesian_forest_plot_plus_stats_ui(id = ns("baseline_risk_forest"))
        )
      )
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
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
    
    # Create forest plot and associated statistics
    bayesian_forest_plot_plus_stats_baseline_risk_server(
      id = "baseline_risk_forest",
      model_reactive = model_reactive,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt
    )
      
    })
}

