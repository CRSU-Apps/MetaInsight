#' Create the covariate analysis panel.
#'
#' @param id ID of the module
#' @param page_numbering PageNumbering object for giving each page a unique identifier in the UI
#' @return Div containing the module UI
covariate_analysis_panel_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
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
            # If a binary covariate is poorly coded, then it will be identified as continuous.
            # Show a warning to the user when covariate is identified as continuous to inform them.
            conditionalPanel(
              condition = "output.inferred_type == 'Continuous'",
              ns = ns,
              div(
                tags$i(class = "fa-solid fa-circle-info"),
                title = "If the covariate is intended to be binary, the only allowed values are 0, 1, and NA",
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
            title = paste0(page_numbering$AddChild(), " Regression plot"),
            covariate_run_model_ui(id = ns("cov_model")),
            regression_plot_panel_ui(id = ns("regression_plot"))
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Forest plot"),
            fixedRow(
              align = "center",
              bayesian_forest_plot_plus_stats_ui(id = ns("cov_forest_plots"))
            )
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Comparison of all treatment pairs"),
            covariate_treatment_comparisons_page_ui(id = ns("cov_treatment_comparisons"))
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Ranking"),
            covariate_ranking_page_ui(id = ns("cov_ranking"))
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Nodesplit model"),
            covariate_nodesplit_page_ui(id = ns("nodesplit"), package_name = "gemtc")
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Result details"),
            result_details_page_ui(id = ns("result_details"), item_names = c("all studies"))
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Deviance report"),
            deviance_report_page_ui(id = ns("deviance_report"), item_names = c("all studies"))
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Model details"),
            model_details_panel_ui(id = ns("model_details"), item_names = c("regression analysis"), page_numbering)
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
    model_reactives <- covariate_run_model_server(
      id = "cov_model",
      data = all_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      covariate = covariate_title,
      cov_friendly = covariate_name,
      model_effects = model_effects
    )
    model_reactive <- model_reactives$model

    covariate_value = covariate_value_panel_server(
      id = "covariate_value",
      covariate_type = reactive({ input$covariate_type_selection }),
      covariate_data = reactive({ all_data()[[covariate_title()]] })
    )
    
    # obtain gemtc output types to be used in rest of page
    model_output <- reactive({
      CovariateModelOutput(
        data = all_data(),
        treatment_ids = treatment_df(),
        model = model_reactive(),
        covariate_title = covariate_title(),
        cov_value = covariate_value(),
        outcome_measure = outcome_measure(),
        covariate_type = covariate_type()
      )
    })
    
    # ReactiveVal contains validity state of the model. NULL if the model has not yet been run.
    model_valid = reactiveVal(NULL)
    parameter_matcher <- ParameterMatcher$new()
    
    # Set validity when model input change
    observe({
      # Only assess the validity once the model has been run the first time
      if (is.null(model_valid())) {
        return()
      }
      
      model_valid(
        parameter_matcher$Matches(
          all_data=all_data(),
          metaoutcome=metaoutcome(),
          outcome_measure=outcome_measure(),
          model_effects=model_effects(),
          regressor_type=model_reactives$regressor_type(),
          covariate_type=covariate_type()
        )
      )
    })
    
    # Record inputs when model run
    observe({
      parameter_matcher$SetParameters(
        all_data=all_data(),
        metaoutcome=metaoutcome(),
        outcome_measure=outcome_measure(),
        model_effects=model_effects(),
        regressor_type=model_reactives$regressor_type(),
        covariate_type=covariate_type()
      )
      model_valid(TRUE)
    }) |> bindEvent(model_reactive())
    
    # Regression plot
    regression_plot_panel_server(
      id = "regression_plot",
      data = all_data,
      covariate_title = covariate_title,
      covariate_name = covariate_name,
      model_output = model_output,
      treatment_df = treatment_df,
      outcome_type = metaoutcome,
      outcome_measure = outcome_measure,
      reference = reference_treatment,
      model_valid = model_valid
    )
    
    # Create forest plot and associated statistics
    bayesian_forest_plot_plus_stats_server(
      id = "cov_forest_plots",
      model_output = model_output,
      analysis_type = "Regression",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt,
      model_valid = model_valid
    )
    
    # 4c-3 Treatment comparisons
    covariate_treatment_comparisons_page_server(
      id = "cov_treatment_comparisons",
      model = model_output,
      outcome_measure = outcome_measure,
      model_valid = model_valid
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
      model_valid = model_valid,
      cov_value = covariate_value
    )
    # 4c-5 Nodesplit model
    covariate_nodesplit_page_server(id = "nodesplit")
    
    # 4c-6 Result details
    result_details_page_server(id = "result_details", models = c(model_output), models_valid = c(model_valid))
    
    # 4c-7 Deviance report
    deviance_report_page_server(id = "deviance_report", models = c(model_output), models_valid = c(model_valid))
    
    # 4c-8 Model details
    model_details_panel_server(id = "model_details", models = c(model_output), models_valid = c(model_valid))
    
  })
}
