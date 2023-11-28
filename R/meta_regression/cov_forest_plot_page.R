
#' Module UI for the covariate forest plots page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
covariate_forest_plots_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Baysesian covariate meta-regression using the gemtc package.",
      br(),
      "Heterogeneity prior: standard deviation ~ U(0,X), where X represents a ",
      tags$i("very large"),
      "difference in the analysis' outcome scale and is determined from the data.",
      br(),
      tags$strong("Please note each simulation may take 20 seconds.", style = "color:#FF0000")
    ),
    fixedRow(
      align = "center",
      p(tags$strong("Results for all studies")),
      p("Please choose your regressor type, then click the button below to run meta-regression analysis (and each time you subsequently change any options)."),
      fluidRow(
        div(selectInput(inputId = ns("select_regressor"), 
                  label = "Choose type of regression coefficient", 
                  choices = c("shared", "unrelated", "exchangeable")),
          style = "display: inline-block;"),
        div(
          shinyWidgets::dropMenu(shinyWidgets::dropdownButton(
            size = 'xs', status = "info", icon=icon('info')), align = 'left',
            p(tags$strong("Types of regressors")),
            p(tags$u("Shared:"), " Coefficient is the same for all treatment comparisons"),
            p(tags$u("Unrelated:"), " Coefficient is different for each treatment comparison"),
            p(tags$u("Exchangeable:"), " Coefficient is different for each treatment comparison but all come from a shared distribution")),
            style = "display:inline-block; vertical-align: top;"
        )
      ),
      actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
    ),
    fixedRow(
      align = "center",
      bayesian_forest_plot_plus_stats_ui(id = ns("all_data"))
    )
  )
}


#' Module server for the covariate forest plots page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#'
#' @return List of reactives: "model_output" contains meta-regression model outputs
covariate_forest_plots_page_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    covariate,
    cov_friendly,
    model_effects,
    bugsnetdt
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Bayesian analysis

    model <- eventReactive(input$baye_do, {
      RunCovariateModel(data = data(), treatment_ids = treatment_df(), outcome_type = metaoutcome(), 
                        outcome = outcome_measure(), covariate = covariate(), cov_friendly = cov_friendly(), 
                        model_type = model_effects(), regressor_type = input$select_regressor, 
                        ref_choice = treatment_df()$Label[match(1, treatment_df()$Number)])
    })
    
    model_output <- reactive(CovariateModelOutput(model = model(), cov_value = NULL))   # once have input, put here

    
    # Create forest plot and associated statistics
    
    bayesian_forest_plot_plus_stats_server(
      id = "all_data",
      model_output = model_output,
      analysis_type = "Regression",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt
    )
    
    
    return(
      list(
        model_output = model_output
      )
    )
  })
}