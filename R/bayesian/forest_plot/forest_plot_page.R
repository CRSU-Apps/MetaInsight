
#' Module UI for the bayesian forest plots page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_forest_plots_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Baysesian result using the gemtc package.",
      br(),
      "Heterogeneity prior: standard deviation ~ U(0,X), where X represents a ",
      tags$i("very large"),
      "difference in the analysis' outcome scale and is determined from the data.",
      br(),
      tags$i("Please note the outcome for continuous data has to be "),
      tags$b("mean difference"),
      tags$i(" for the Bayesian analysis. Standardised mean difference cannot be analysed."),
      br(),
      tags$i("Please note the outcome for binary data has to be "),
      tags$b("Odds Ratio or Risk Ratio"),
      tags$i(" for the Bayesian analysis. Risk difference cannot be analysed."),
      tags$strong("Please note each simulation may take 20 seconds.", style = "color:#FF0000")
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        p(tags$strong("Results for all studies")),
        actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
      ),
      column(
        width = 6,
        align = "center",
        p(tags$strong("Results with selected studies excluded")),
        actionButton(inputId = ns("sub_do"), label = "Click here to run the sensitivity analysis")
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        bayesian_forest_plot_plus_stats_ui(id = ns("all_data"))
      ),
      column(
        width = 6,
        align = "center",
        bayesian_forest_plot_plus_stats_ui(id = ns("sub_data"))
      )
    )
  )
}


#' Module server for the bayesian forest plots page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param sensitivity_data Reactive containing data to analyse for sensitivity analysis
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param sensitivity_treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label) for sensitivity analysis
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param bugsnetdt_sub Reactive containing bugsnet meta-analysis for sensitivity analysis
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
#'
#' @return List of reactives:
#' - "model" contains the full bayesian meta-analysis
#' - "model_sub" contains the bayesian meta-analysis with studies excluded
#' - "model_valid" contains the validity of the full bayesian meta-analysis, NULL if the analysis has not been run yet
#' - "model_sub_valid" contains the validity of the bayesian meta-analysis with studies excluded, NULL if the analysis has not been run yet
bayesian_forest_plots_page_server <- function(
    id,
    data,
    sensitivity_data,
    treatment_df,
    sensitivity_treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    bugsnetdt,
    bugsnetdt_sub,
    reference_alter
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### SMD & RD warning alert

    observeEvent(list(input$baye_do, input$sub_do), {
      if (outcome_measure()=="SMD") {
        showNotification("Please note: standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight", type = "error", duration = NULL)
      }
      else if (outcome_measure()=="RD") {
        showNotification("Please note: Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight", type = "error", duration = NULL)
      }
    })

    # Bayesian analysis

    model <- eventReactive(input$baye_do, {
      bayesian_model(data(), treatment_df(), metaoutcome(),
                     outcome_measure(), model_effects(), reference_alter()$ref_all)
    })

    model_sub <- eventReactive(input$sub_do, {
      bayesian_model(sensitivity_data(), sensitivity_treatment_df(), metaoutcome(),
                     outcome_measure(), model_effects(), reference_alter()$ref_sub)
    })
    
    # ReactiveVals contain validity state of the model. NULL if the model has not yet been run.
    model_valid = reactiveVal(NULL)
    model_sub_valid = reactiveVal(NULL)
    parameter_matcher <- ParameterMatcher$new()
    parameter_matcher_sub <- ParameterMatcher$new()
    
    # Set validity when model input change
    observe({
      # Only assess the validity once the model has been run the first time
      if (is.null(model_valid())) {
        return()
      }
      
      model_valid(
        parameter_matcher$Matches(
          treatment_df = treatment_df(),
          metaoutcome = metaoutcome(),
          outcome_measure = outcome_measure(),
          model_effects = model_effects(),
          ref = reference_alter()$ref_all
        )
      )
    })
    
    # Set validity when model input change
    observe({
      # Only assess the validity once the model has been run the first time
      if (is.null(model_sub_valid())) {
        return()
      }
      
      model_sub_valid(
        parameter_matcher_sub$Matches(
          treatment_df = sensitivity_treatment_df(),
          metaoutcome = metaoutcome(),
          outcome_measure = outcome_measure(),
          model_effects = model_effects(),
          ref = reference_alter()$ref_sub
        )
      )
    })
    
    # Clear validity when data changes
    observe({
      model_valid(NULL)
    }) |> bindEvent(data())
    
    # Clear validity when data changes
    observe({
      model_sub_valid(NULL)
    }) |> bindEvent(sensitivity_data())
    
    # Record inputs when model run
    observe({
      parameter_matcher$SetParameters(
        treatment_df = treatment_df(),
        metaoutcome = metaoutcome(),
        outcome_measure = outcome_measure(),
        model_effects = model_effects(),
        ref = reference_alter()$ref_all
      )
      model_valid(TRUE)
    }) |> bindEvent(model())
    
    # Record inputs when model run
    observe({
      parameter_matcher_sub$SetParameters(
        treatment_df = sensitivity_treatment_df(),
        metaoutcome = metaoutcome(),
        outcome_measure = outcome_measure(),
        model_effects = model_effects(),
        ref = reference_alter()$ref_sub
      )
      model_sub_valid(TRUE)
    }) |> bindEvent(model_sub())
    
    
    # Create forest plot and associated statistics
    
    bayesian_forest_plot_plus_stats_server(
      id = "all_data",
      model_output = model,
      analysis_type = "Full",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt,
      model_valid = model_valid
    )
    
    bayesian_forest_plot_plus_stats_server(
      id = "sub_data",
      model_output = model_sub,
      analysis_type = "Sub",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt_sub,
      model_valid = model_sub_valid
    )
    

    output$ref_change_bay <- renderText({
      if (identical(reference_alter()$ref_sub, reference_alter()$ref_all)=="FALSE") {
        paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", reference_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
      }
    })
    
    return(
      list(
        model = model,
        model_sub = model_sub,
        model_valid = model_valid,
        model_sub_valid = model_sub_valid
      )
    )
  })
}