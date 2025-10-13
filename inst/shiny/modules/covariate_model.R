covariate_model_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI
    #selectInput(ns("dataset"), "Dataset", choices = c("All studies" = "all" ,"With selected studies excluded" = "sub" )),
    sliderInput(ns("covariate_value"), "Covariate value", min = 0, max = 100, step = 1, value = 50),
    radioButtons(ns("regressor"), "Regression coefficient",
                 choiceNames = list(add_tooltip("Shared", "Coefficient is the same for all treatment comparisons"),
                                    add_tooltip("Exchangable", "Coefficient is different for each treatment comparison but all come from a shared distribution"),
                                    add_tooltip("Unrelated", "Coefficient is different for each treatment comparison")),
                 choiceValues = list("shared", "exchangable", "unrelated")),
    input_task_button(ns("run"), "Run model", type = "default", icon = icon("arrow-turn-down"))

  )
}

covariate_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    observe({
      watch("setup_configure")
      req(common$main_connected_data)
      # these could be moved to setup_configure and then not be exported
      common$covariate_column <- FindCovariateNames(common$main_connected_data)
      common$covariate_name <- GetFriendlyCovariateName(common$covariate_column)
      min <- min(common$main_connected_data[[common$covariate_column]])
      mean <- mean(common$main_connected_data[[common$covariate_column]])
      max <- max(common$main_connected_data[[common$covariate_column]])
      updateSliderInput(session, "covariate_value", min = min, max = max, value = mean, label = glue::glue("Covariate value ({common$covariate_name})"))
    })

    # used to trigger summary table - needs to be separate to reload
    init("covariate_model_table")
    # used to trigger when model is fitted
    init("covariate_model_fit")

    # blank the output when the regressor is updated thereby
    # refitting the whole model
    observe({
      input$regressor
      common$covariate_model <- NULL
    })

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", "Please configure the analysis in the Setup section first.")
        return()
      }

      if (is.null(common$covariate_column)){
        common$logger |> writeLog(type = "error", paste("No covariate data exists. To add covariate data, add a column titled",
        code("covar.*"), "where the", code("*"), "is replaced by the covariate name. eg. ", code("covar.age")))
        return()
      }

      if (common$outcome_measure == "SMD") {
        common$logger |> writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
        return()
      }
      else if (common$outcome_measure == "RD") {
        common$logger |> writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
        return()
      }
      # METADATA ####
      common$meta$covariate_model$used <- TRUE
      # common$meta$covariate_model$dataset <- input$dataset
      common$meta$covariate_model$covariate_value <- as.numeric(input$covariate_value)
      common$meta$covariate_model$regressor <- input$regressor

      trigger("covariate_model")
    })

    common$tasks$covariate_model <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = covariate_model, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("covariate_model"), watch("model"), input$covariate_value, input$regressor), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch("covariate_model") > 0 || all(!is.null(common$covariate_model), watch("model") > 0)))

      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "starting", "Fitting covariate model")
      } else {
        common$logger |> writeLog(type = "starting", "Updating covariate model")
      }
      common$tasks$covariate_model$invoke(common$main_connected_data,
                                          common$treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$covariate_column,
                                          common$covariate_name,
                                          input$covariate_value,
                                          common$model_type,
                                          input$regressor,
                                          common$reference_treatment_all,
                                          common$covariate_model,
                                          async = TRUE)
      model_result$resume()
    })

    model_result <- observe({

      result <- common$tasks$covariate_model$result()
      model_result$suspend()
      if (inherits(result, "bayes_model")){
        common$covariate_model <- result
        common$covariate_value <- common$meta$covariate_model$covariate_value
        shinyjs::runjs("Shiny.setInputValue('covariate_model-complete', 'complete');")
        common$logger |> writeLog(type = "complete", "Covariate model has been fitted")
      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger("covariate_model_table")
      trigger("covariate_model_fit")
    })

  output$table <- renderTable({
    watch("covariate_model_table")
    req(common$covariate_model)
    common$covariate_model$dic
  }, digits = 3, rownames = TRUE, colnames = FALSE)

})
}


covariate_model_module_result <- function(id) {
  ns <- NS(id)
  tableOutput(ns("table"))
}

covariate_model_module_rmd <- function(common){ list(
  covariate_model_knit = !is.null(common$meta$covariate_model$used),
  # covariate_model_dataset = common$meta$covariate_model$dataset,
  covariate_model_covariate_column = common$covariate_column,
  covariate_model_covariate_name = common$covariate_name,
  covariate_model_covariate_value = common$meta$covariate_model$covariate_value,
  covariate_model_regressor = common$meta$covariate_model$regressor)
}

