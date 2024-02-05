#' Create the covariate analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
covariate_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      div(
        h2(textOutput(outputId = ns("subtitle"))),
        style = "display: inline-block; vertical-align: top; padding-right: 20pt;"
      ),
      div(
        # Type selection if the covariate data is valid
        conditionalPanel(
          condition = "output.valid_covariate",
          ns = ns,
          div(
            selectInput(
              inputId = ns("covariate_type_selection"),
              label = "",
              choices = c("Binary", "Continuous"),
              width = "120pt"
            ),
            style = "display: inline-block;"
          ),
          div(
            # If binary data is poorly coded, then it will be identified as continuous.
            # Show a warning to the user when data is identified as continuous to inform them.
            conditionalPanel(
              condition = "output.inferred_type == 'Continuous'",
              ns = ns,
              div(
                tags$i(class = "fa-solid fa-circle-info"),
                title = "If the data is intended to be binary, the only allowed values are 0, 1, and NA",
                style = "color: orange;"
              )
            ),
            style = "display: inline-block; vertical-align: 65%"
          ),
          div(
            covariate_value_panel_ui(id = ns("covariate_value")),
            style = "display: inline-block; vertical-align: 65%"
          )
        ),
        style = "display: inline-block;"
      ),
      # Show error message if invalid data
      conditionalPanel(
        condition = "!output.valid_covariate",
        ns = ns,
        div(
          textOutput(outputId = ns("error_message_box")),
          style = "display: inline-block; color: red; font-style: italic; font-weight: bold; padding-right: 20pt;"
        ),
        style = "vertical-align: 65%"
      ),
      # Meta-regression UI
      conditionalPanel(
        condition = "output.valid_covariate",
        ns = ns,
        tabsetPanel(
          tabPanel(
            title = "4c-1. Forest plot",
            covariate_run_model_ui(id = ns("cov_model")),
            fixedRow(
              align = "center",
              bayesian_forest_plot_plus_stats_ui(id = ns("cov_forest_plots"))
            )
          ),
          tabPanel(
            title = "4c-2. Regression plot",
            regression_plot_panel_ui(id = ns("regression_plot"))
          ),
          tabPanel(
            title = "4c-3. Comparison of all treatment pairs",
            covariate_treatment_comparisons_page_ui(id = ns("cov_treatment_comparisons"))
          ),
          tabPanel(
            title = "4c-4. Ranking",
            covariate_ranking_page_ui(id = ns("cov_ranking"))
          ),
          tabPanel(
            title = "4c-5. Nodesplit model",
            covariate_nodesplit_page_ui(id = ns("nodesplit"), package_name = "gemtc")
          )
        )
      )
    )
  )
}

#' Create the covariate analysis server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel)
#' @param reference_treatment Reactive containing the sanitised name of the reference treatment
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
covariate_analysis_panel_server <- function(
    id, 
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
    
    covariate_title <- reactive({
      FindCovariateNames(all_data())[1]
    })
    
    covariate_name <- reactive({
      GetFriendlyCovariateName(covariate_title())
    })
    
    covariate_type <- reactive({
      input$covariate_type_selection
    })
    
    output$subtitle <- renderText({
      return(glue::glue("Covariate: {covariate_name()}"))
    })
    
    error_message <- reactiveVal("")
    output$error_message_box <- renderText({ error_message() })
    
    inferred_type <- reactiveVal()
    
    # Try to infer the type of the covariate, and display an error to the user if the data is bad
    observe({
      tryCatch(
        {
          inferred_type <- ValidateAndInferCovariateType(all_data(), covariate_title())
          shiny::updateSelectInput(inputId = "covariate_type_selection", selected = inferred_type)
          inferred_type(inferred_type)
          if (inferred_type == "Continuous") {
            shinyjs::disable(id = "covariate_type_selection")
          } else {
            shinyjs::enable(id = "covariate_type_selection")
          }
          error_message("")
        },
        error = function(exptn) {
          inferred_type(NULL)
          error_message(exptn$message)
        }
      )
    })
    
    output$valid_covariate <- reactive({ error_message() == "" })
    outputOptions(x = output, name = "valid_covariate", suspendWhenHidden = FALSE)
    
    
    output$inferred_type <- reactive({ inferred_type() })
    outputOptions(x = output, name = "inferred_type", suspendWhenHidden = FALSE)
    
    # 4c-1 Forest plots
    # run model
    model_reactive <- covariate_run_model_server(
      id = "cov_model",
      data = all_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      covariate = covariate_title,
      cov_friendly = covariate_name,
      model_effects = model_effects
    )

    covariate_value = covariate_value_panel_server(
      id = "covariate_value",
      covariate_type = reactive({ input$covariate_type_selection }),
      covariate_data = reactive({ all_data()[[covariate_title()]] })
    )
    
    # obtain gemtc output types to be used in rest of page
    model_output <- reactive({
      m_output <- CovariateModelOutput(model = model_reactive(), cov_value = covariate_value())
      return(m_output)
      })
    
    # Create forest plot and associated statistics
    bayesian_forest_plot_plus_stats_server(
      id = "cov_forest_plots",
      model_output = model_output,
      analysis_type = "Regression",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt
    )
    
    # 4c-2 Regression plot
    regression_plot_panel_server(
      id = "regression_plot",
      model = model_reactive,
      reference_treatment = reference_treatment,
      treatment_df = treatment_df,
      covariate_value = covariate_value
    )
    
    # 4c-3 Treatment comparisons
    covariate_treatment_comparisons_page_server(
      id = "cov_treatment_comparisons",
      model = model_output,
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
    
    # 4c-5 Nodesplit model
    covariate_nodesplit_page_server(id = "nodesplit")

  })
}
