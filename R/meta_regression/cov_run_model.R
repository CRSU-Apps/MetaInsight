
#' Module UI for the running model part of covariate forest plots page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
covariate_run_model_ui <- function(id) {
  ns <- NS(id)
  div(
    "Baysesian covariate meta-regression using the", textOutput(ns("package"), inline=TRUE), "package.",
    br(),
    "Heterogeneity prior: standard deviation", textOutput(ns("model_text"), inline=TRUE),
    br(),
    tags$strong("Please note each simulation may take 60 seconds.", style = "color:#FF0000"),

    fixedRow(
      align = "center",
      p(tags$strong("Results for all studies")),
      p("Please choose your regressor type, then click the button below to run meta-regression analysis (and each time you subsequently change any options)."),
      fluidRow(
        div(selectInput(inputId = ns("select_regressor"), 
                  label = "Choose type of regression coefficient", 
                  choices = c("shared", "exchangeable", "unrelated")),
          style = "display: inline-block;"),
        div(
          shinyWidgets::dropMenu(shinyWidgets::dropdownButton(
            size = 'xs', status = "info", icon=icon('info')), align = 'left',
            p(tags$strong("Types of regressors")),
            p(tags$u("Shared:"), " Coefficient is the same for all treatment comparisons"),
            p(tags$u("Exchangeable:"), " Coefficient is different for each treatment comparison but all come from a shared distribution"),
            p(tags$u("Unrelated:"), " Coefficient is different for each treatment comparison")),
            style = "display:inline-block; vertical-align: top;"
        )
      ),
      actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
    )
  )
}




#' Module server for running the covariate model.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#'
#' @return List of reactives: "model_output" contains meta-regression model outputs
covariate_run_model_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    covariate,
    cov_friendly,
    model_effects
    ) {
  moduleServer(id, function(input, output, session) {

    #Bayesian analysis
    
    output$package <- renderText({"gemtc"})
    output$model_text <- renderText({
      "~ U(0,X), where X represents a very large difference in the analysis' outcome scale and is determined from the data."
    })
    
    
    #The model
    model <- eventReactive(input$baye_do, {
      RunCovariateModel(
        data = data(),
        treatment_ids = treatment_df(),
        outcome_type = metaoutcome(),
        outcome = outcome_measure(),
        covariate = covariate(),
        cov_friendly = cov_friendly(),
        model_type = model_effects(),
        regressor_type = input$select_regressor,
        ref_choice = treatment_df()$Label[match(1, treatment_df()$Number)]
      )
    })

    return(
      list(
        model = model,
        regressor = reactive({ input$select_regressor })
      )
    )
  })
}





#' Module server for running the baseline risk model.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#'
#' @return List of reactives: "model_output" contains meta-regression model outputs
baseline_risk_run_model_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    model_effects
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$package <- renderText({"bnma"})
    output$model_text <- renderText({
      "~ U(0,5) or U(0,100) for a binary or continuous outcome respectively."
    })
    
    #The model
    model <- eventReactive(input$baye_do, {
      BaselineRiskRegression(br_data = data(),
                             treatment_ids = treatment_df(),
                             outcome_type = metaoutcome(),
                             ref = treatment_df()$Label[match(1, treatment_df()$Number)],
                             effects_type = model_effects(),
                             cov_parameters = input$select_regressor)
    })
    
    return(
      list(
        model = model,
        regressor = reactive({ input$select_regressor })
      )
    )
  })
}