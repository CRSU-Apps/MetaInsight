
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
      actionButton(
        inputId = ns("baye_do"),
        label = span(
          "Click here to run the main analysis for all studies",
          # Initially hidden spinner
          div(
            id = ns("spinner"),
            tags$i(class = "fa-solid fa-circle-notch fa-spin"),
            style = "color: blue; padding-left: 5pt; display: none;"
          ),
          style="display: flex;"
        )
      )
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
#' @return List of reactives:
#' - "model" contains meta-regression model outputs from `RunCovariateModel()`.
#' - "regressor" caontins the type of the regression model. One of ["shared", "exchangeable", "unrelated"].
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
    
    # Run model as a parallel process
    model <- shiny::ExtendedTask$new(function(data, treatment_ids, outcome_type, outcome, covariate, cov_friendly, model_type, regressor_type, ref_choice) {
      promises::future_promise({
        RunCovariateModel(
          data = data,
          treatment_ids = treatment_ids,
          outcome_type = outcome_type,
          outcome = outcome,
          covariate = covariate,
          cov_friendly = cov_friendly,
          model_type = model_type,
          regressor_type = regressor_type,
          ref_choice = ref_choice
        )
      })
    })
    
    # Kick off the model calculation when the button is pressed
    observe({
      model$invoke(
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
    }) |>
      bindEvent(input$baye_do)
    
    # When button is clicked, show the spinner and disable the button
    observe({
      shinyjs::show(id = "spinner")
      shinyjs::disable(id = "baye_do")
    }) |>
      bindEvent(input$baye_do)
    
    # When model calculation completes, hide the spinner and enable the button
    observe({
      shinyjs::hide(id = "spinner")
      shinyjs::enable(id = "baye_do")
    }) |>
      bindEvent(model$result())
    
    return(
      list(
        model = reactive({ model$result() }),
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
#' @return List of reactives:
#' - "model" contains meta-regression model outputs from `BaselineRiskRegression()`.
#' - "regressor" caontins the type of the regression model. One of ["shared", "exchangeable", "unrelated"].
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
    
    # Run model as a parallel process
    model <- shiny::ExtendedTask$new(function(data, treatment_ids, outcome_type, ref, effects_type, cov_parameters) {
      promises::future_promise({
        BaselineRiskRegression(br_data = data,
                               treatment_ids = treatment_ids,
                               outcome_type = outcome_type,
                               ref = ref,
                               effects_type = effects_type,
                               cov_parameters = cov_parameters)
      })
    })
    
    # Kick off the model calculation when the button is pressed
    observe({
      model$invoke(
        data = data(),
        treatment_ids = treatment_df(),
        outcome_type = metaoutcome(),
        ref = treatment_df()$Label[match(1, treatment_df()$Number)],
        effects_type = model_effects(),
        cov_parameters = input$select_regressor
      )
    }) |>
      bindEvent(input$baye_do)
    
    # When button is clicked, show the spinner and disable the button
    observe({
      shinyjs::show(id = "spinner")
      shinyjs::disable(id = "baye_do")
    }) |>
      bindEvent(input$baye_do)
    
    # When model calculation completes, hide the spinner and enable the button
    observe({
      shinyjs::hide(id = "spinner")
      shinyjs::enable(id = "baye_do")
    }) |>
      bindEvent(model$result())
    
    return(
      list(
        model = reactive({ model$result() }),
        regressor = reactive({ input$select_regressor })
      )
    )
  })
}