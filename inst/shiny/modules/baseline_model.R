baseline_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("regressor"), "Regression coefficient",
                 choiceNames = list(add_tooltip("Shared", "Coefficient is the same for all treatment comparisons"),
                                    add_tooltip("Exchangable", "Coefficient is different for each treatment comparison but all come from a shared distribution"),
                                    add_tooltip("Unrelated", "Coefficient is different for each treatment comparison")),
                 choiceValues = list("shared", "exchangeable", "unrelated")),
    input_task_button(ns("run"), "Run model", type = "default", icon = icon("arrow-turn-down")),
    div(class = "baseline_model_div",
        actionButton(ns("run_all"), "Run all modules", icon = icon("forward-fast"))
    )
  )
}

baseline_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id, show = FALSE)

    # used to trigger summary table - needs to be separate to reload
    init("baseline_model_table")
    # used to trigger when model is fitted
    init("baseline_model_fit")

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis in the Setup section first")
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
                                         common$outcome_measure,
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

        if (common$baseline_model$mtcResults$max.gelman > 1.05){
          common$logger |> writeLog(type = "warning", glue::glue(
            "The Gelman-Rubin statistic is {round(common$baseline_model$mtcResults$max.gelman, 2)}.
            A value greater than 1.05 may indicate lack of convergence. Please check the Gelman plot in the
            Deviance report module"))
        }

      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger("baseline_model_table")
      trigger("baseline_model_fit")
    })

    output$table <- renderTable({
      watch("baseline_model_table")
      req(common$baseline_model)
      shinyjs::show(selector = ".baseline_model_div")
      common$baseline_model$dic
      }, digits = 3, rownames = TRUE, colnames = FALSE)

    observeEvent(input$run_all, {
      run_all("baseline", common$logger)
    })

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
      regressor = input$regressor)
      },
      load = function(state) {
        ### Manual load start
        ### Manual load end
      updateRadioButtons(session, "regressor", selected = state$regressor)
      }
    ))
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

