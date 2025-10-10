baseline_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("regressor"), "Regression coefficient",
                choices = c("Shared" = "shared",
                            "Exchangable" = "exchangeable",
                            "Unrelated" = "unrelated")),
    input_task_button(ns("run"), "Run model", type = "default", icon = icon("arrow-turn-down"))
  )
}

baseline_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


    # used to trigger summary table - needs to be separate to reload
    init("baseline_model_table")
    # used to trigger when model is fitted
    init("baseline_model_fit")

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", "Please configure the analysis in the Setup section first.")
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
      common$meta$baseline_model$used <- TRUE
      # common$meta$covariate_model$dataset <- input$dataset
      common$meta$baseline_model$regressor <- input$regressor

      trigger("baseline_model")
    })

    common$tasks$baseline_model <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = baseline_model, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("baseline_model"), watch("model"), input$regressor), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch("baseline_model") > 0 || all(!is.null(common$baseline_model), watch("model") > 0)))

      if (is.null(common$baseline_model)){
        common$logger |> writeLog(type = "starting", "Fitting baseline model")
      } else {
        common$logger |> writeLog(type = "starting", "Updating baseline model")
      }

      common$tasks$baseline_model$invoke(common$main_connected_data,
                                         common$treatment_df,
                                         common$outcome,
                                         common$reference_treatment_all,
                                         common$model_type,
                                         input$regressor,
                                         common$seed,
                                         async = TRUE)
      model_result$resume()
    })

    model_result <- observe({

      result <- common$tasks$baseline_model$result()
      model_result$suspend()
      if (inherits(result, "baseline_model")){
        common$baseline_model <- result
        shinyjs::runjs("Shiny.setInputValue('baseline_model-complete', 'complete');")
        common$logger |> writeLog(type = "complete", "Baseline model has been fitted")
      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger("baseline_model_table")
      trigger("baseline_model_fit")
    })

    output$table <- renderTable({
      watch("baseline_model_table")
      req(common$baseline_model)
      common$baseline_model$dic
      }, digits = 3, rownames = TRUE, colnames = FALSE)

})
}


baseline_model_module_result <- function(id) {
  ns <- NS(id)
  tableOutput(ns("table"))
}

baseline_model_module_rmd <- function(common) {list(
  baseline_model_knit = !is.null(common$meta$baseline_model$used),
  baseline_model_regressor = common$meta$baseline_model$regressor)
}

