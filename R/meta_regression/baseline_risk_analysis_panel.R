#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  ui = fluidPage(
    div(
      baseline_risk_value_panel_ui(id = ns("baseline_risk_value")),
      style = "display: inline-block; vertical-align: 65%"
    ),
    tabsetPanel(
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Regression plot"),
        covariate_run_model_ui(id = ns("baseline_risk_model")),
        regression_plot_panel_ui(id = ns("baseline_risk_regression_plot"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Forest plot"),
        fixedRow(
          align = "center",
          uiOutput(outputId = ns("convergence_warning")),
          bayesian_forest_plot_plus_stats_ui(id = ns("baseline_risk_forest"))
        )
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Comparison of all treatment pairs"),
        covariate_treatment_comparisons_page_ui(id = ns("baseline_risk_treatment_comparisons"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Ranking"),
        covariate_ranking_page_ui(id = ns("baseline_risk_ranking"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Nodesplit"),
        covariate_nodesplit_page_ui(id = ns("baseline_risk_nodesplit"), package_name = "bnma")
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Result details"),
        result_details_page_ui(id = ns("baseline_risk_result_details"), item_names = c("all studies"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Deviance report"),
        deviance_report_page_ui(id = ns("baseline_risk_deviance_report"), item_names = c("all studies"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Model details"),
        model_details_panel_ui(id = ns("baseline_risk_model_details"), item_names = c("regression analysis"), page_numbering)
      )
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}






#' Create the baseline risk analysis server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference_treatment Reactive containing the sanitised name of the reference treatment.
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "OR, or "RR".
#' @param model_effects Reactive containing model effects: either "random" or "fixed".
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not.
#' @param freq_all Reactive containing frequentist meta-analysis.
#' @param bugsnetdt Reactive containing bugsnet meta-analysis.
baseline_risk_analysis_panel_server <- function(    id, 
                                                    all_data,
                                                    treatment_df,
                                                    reference_treatment,
                                                    metaoutcome,
                                                    outcome_measure,
                                                    model_effects,
                                                    rank_option,
                                                    freq_all,
                                                    bugsnetdt
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    long_data <- reactive({
      if (FindDataShape(all_data()) == "wide") {
        return(WideToLong(all_data(), outcome_type = metaoutcome()))
      } else {
        return(all_data())
      }
    })
    
    model_reactives <- baseline_risk_run_model_server(
      id = "baseline_risk_model",
      data = long_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      model_effects = model_effects
    )
    model_reactive <- model_reactives$model
    
    #Warn if the model did not converge
    output$convergence_warning <- renderText({
        ifelse(model_reactive()$max.gelman > 1.05,
               paste0("<b><h3> Warning: the Gelman-Rubin statistic is ",
                      round(model_reactive()$max.gelman, digits = 2),
                      ". A value greater than 1.05 may indicate lack of convergence. Please check the Gelman plots in the <i>Result details</i> tab.</h3></b>"),
               "")
    })
    
    #Covariate value for displaying at the top of the page, calculated from the observed reference outcomes
    baseline_risk_value_panel_server(
      id = "baseline_risk_value",
      model = model_reactive()
    )
    
    #Obtain output to create the graph
    model_output <- reactive({
      BaselineRiskModelOutput(data = long_data(),
                              treatment_ids = treatment_df(),
                              model = model_reactive(),
                              outcome_measure = outcome_measure()
      )
    })
    
    #The covariate values for use in the graph, including imputed reference outcomes
    reference_outcome <- reactive({
      GetReferenceOutcome(data = long_data(),
                          treatment_ids = treatment_df(),
                          outcome_type = metaoutcome(),
                          observed = "Imputed",
                          model = model_reactive())
    })
    
    #Remove any covariates already in the data set and add the outcome in the reference arm as the unique covariate
    data_with_covariate <- reactive({
      data_with_covariates_removed <- dplyr::select(long_data(), !dplyr::starts_with("covar."))
      return(merge(data_with_covariates_removed,
                   data.frame(Study = names(reference_outcome()),
                              covar.baseline_risk = reference_outcome(),
                              row.names = NULL),
                   by = "Study"
                   )
             )
    })
    
    model_valid = reactiveVal(FALSE)
    parameter_matcher <- ParameterMatcher$new()
    
    observe({
      model_valid(
        parameter_matcher$Matches(
          all_data=all_data(),
          metaoutcome=metaoutcome(),
          outcome_measure=outcome_measure(),
          model_effects=model_effects(),
          rank_option=rank_option(),
          regressor=model_reactives$regressor_type()
        )
      )
    })
    
    observe({
      parameter_matcher$SetParameters(
        all_data=all_data(),
        metaoutcome=metaoutcome(),
        outcome_measure=outcome_measure(),
        model_effects=model_effects(),
        rank_option=rank_option(),
        regressor=model_reactives$regressor_type()
      )
      model_valid(TRUE)
    }) |> bindEvent(model_reactive())
    
    # 4b-1 Regression plot
    regression_plot_panel_server(
      id = "baseline_risk_regression_plot",
      data = data_with_covariate,
      covariate_title = reactive("covar.baseline_risk"),
      covariate_name = reactive("Baseline Risk"),
      model_output = model_output,
      treatment_df = treatment_df,
      outcome_type = metaoutcome,
      outcome_measure = outcome_measure,
      reference = reference_treatment,
      package = "bnma",
      model_valid = model_valid
    )
    
    # 4b-2 Create forest plot and associated statistics
    bayesian_forest_plot_plus_stats_baseline_risk_server(
      id = "baseline_risk_forest",
      model_reactive = model_reactive,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt
    )
    
    # 4b-3 Treatment comparisons
    treatment_comparisons_page_baseline_risk_server(
      id = "baseline_risk_treatment_comparisons",
      model = model_reactive,
      outcome_measure = outcome_measure
    )
    
    # 4b-4 Ranking Panel
    covariate_ranking_page_server(
      id = "baseline_risk_ranking",
      model = model_reactive,
      data = long_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      model_effects = model_effects,
      rank_option = rank_option,
      freq_all = freq_all,
      bugsnetdt = bugsnetdt,
      package = "bnma"
    )
      
    # 4b-5 Nodesplit model
    covariate_nodesplit_page_server(id = "baseline_risk_nodesplit")
    
    # 4b-6 Result details
    result_details_page_server(id = "baseline_risk_result_details", models = c(model_reactive), package = "bnma")
    
    # 4b-7 Deviance report
    deviance_report_page_server(id = "baseline_risk_deviance_report", models = c(model_reactive), package = "bnma")
    
    # 4c-8 Model details
    model_details_panel_server(id = "baseline_risk_model_details", models = c(model_reactive), package = "bnma")
    
    })
}

