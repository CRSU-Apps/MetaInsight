covariate_model_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("covariate_value"), "Covariate value", min = 0, max = 100, step = 1, value = 50),
    radioButtons(ns("regressor"), "Regression coefficient",
                 choiceNames = list(add_tooltip("Shared", "Coefficient is the same for all treatment comparisons"),
                                    add_tooltip("Exchangeable", "Coefficient is different for each treatment comparison but all come from a shared distribution"),
                                    add_tooltip("Unrelated", "Coefficient is different for each treatment comparison")),
                 choiceValues = list("shared", "exchangeable", "unrelated")),
    input_task_button(ns("run"), "Fit model", type = "default", icon = icon("arrow-turn-down")),
    div(class = "covariate_model_div download_buttons",
        actionButton(ns("run_all"), "Run all modules", icon = icon("forward-fast"))
    )
  )
}

covariate_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id, show = FALSE)

    # update slider with relevant values
    observe({
      watch("setup_configure")
      req(common$covariate_column)
      if (!is.null(common$covariate_column)){
        min <- min(common$main_connected_data[[common$covariate_column]])
        mean <- mean(common$main_connected_data[[common$covariate_column]])
        max <- max(common$main_connected_data[[common$covariate_column]])
        log_val <- round(log10(max - min))
        label <- glue("Covariate value ({common$covariate_name})")
        if (common$covariate_type == "Continuous"){
          step <- 10 ** (log_val - 2)
          updateSliderInput(session, "covariate_value", min = min, max = max, value = mean, step = step, label = label)
        }
        if (common$covariate_type == "Binary"){
          step <- 1
          # hide ticks
          on.exit(shinyjs::delay(100, shinyjs::runjs("$('#covariate_model-covariate_value').siblings('.irs').find('.irs-grid').hide();")))
          updateSliderInput(session, "covariate_value", min = min, max = max, value = 0, step = step, label = label)
        }
      }
    })

    observe({
      watch("setup_reset")
      # show ticks
      on.exit(shinyjs::delay(100, shinyjs::runjs("$('#covariate_model-covariate_value').siblings('.irs').find('.irs-grid').show();")))
      updateSliderInput(session, "covariate_value", label = "Covariate value", min = 0, max = 100, step = 1, value = 50)
    })

    # used to trigger summary table - needs to be separate to reload
    init("covariate_model_table")
    # used to trigger when model is fitted
    init("covariate_model_fit")

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis in the Setup section first.")
        return()
      }

      if (is.null(common$covariate_column)){
        common$logger |> writeLog(type = "error", paste("No covariate data exists. To add covariate data, add a column titled
                                                         covar.* where the * is replaced by the covariate name. e.g. covar.age"))
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
      common$meta$covariate_model$covariate_value <- as.numeric(input$covariate_value)
      common$meta$covariate_model$regressor <- input$regressor

      trigger("covariate_model")
    })

    # needed to cancel in progress
    cov_model <- NULL
    common$tasks$covariate_model <- ExtendedTask$new(
      function(...) cov_model <<- mirai::mirai(run(...), run = covariate_model, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("covariate_model"), watch("model"), debounce(input$covariate_value, 1000), input$regressor), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch("covariate_model") > 0 || all(!is.null(common$covariate_model), watch("model") > 0)))

      # cancel if the model is already updating
      if (common$tasks$covariate_model$status() == "running"){
        mirai::stop_mirai(cov_model)
      }

      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "starting", "Fitting covariate model")
      } else {
        common$logger |> writeLog(type = "starting", "Updating covariate model")
      }
      common$tasks$covariate_model$invoke(common$main_connected_data,
                                          common$treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          input$covariate_value,
                                          common$model_type,
                                          input$regressor,
                                          common$covariate_type,
                                          common$reference_treatment_all,
                                          common$seed,
                                          common$covariate_model,
                                          async = TRUE)
      model_result$resume()
    })

    model_result <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$covariate_model$status() == "success"){
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
      }
    })

  output$table <- renderTable({
    watch("covariate_model") # required for reset
    watch("covariate_model_table")
    req(common$covariate_model)
    shinyjs::show(selector = ".covariate_model_div")
    common$covariate_model$dic
  }, digits = 3, rownames = TRUE, colnames = FALSE)

  outputOptions(output, "table", suspendWhenHidden = FALSE)

  observeEvent(input$run_all, {
    run_all("covariate", common$logger)
  })

  return(list(
    save = function() {
      # only save covariate info when it exists
      if (is.null(common$covariate_column)){
        list()
      } else {
        covariate_min = min(common$main_connected_data[[common$covariate_column]])
        covariate_max = max(common$main_connected_data[[common$covariate_column]])
        log_val <- round(log10(covariate_max - covariate_min))
        step <- 10 ** (log_val - 2)
        list(
          ### Manual save start
          covariate_min = covariate_min,
          covariate_max = covariate_max,
          covariate_step = ifelse(common$covariate_type == "Continuous", step, 1),
          covariate_label = glue("Covariate value ({common$covariate_name})"),
          covariate_tick = ifelse(common$covariate_type == "Continuous", TRUE, FALSE),
          ### Manual save end
          covariate_value = input$covariate_value,
          regressor = input$regressor)
      }
    },
    load = function(state) {
      if (!is.null(state$covariate_min)){
        ### Manual load start
        updateSliderInput(session, "covariate_value",
                          min = state$covariate_min,
                          max = state$covariate_max,
                          value = state$covariate_value,
                          step = state$covariate_step,
                          label = state$covariate_label
                          )
        if (!state$covariate_tick){
          # hide ticks
          on.exit(shinyjs::delay(100, shinyjs::runjs("$('#covariate_model-covariate_value').siblings('.irs').find('.irs-grid').hide();")))
        }
        ### Manual load end
        updateRadioButtons(session, "regressor", selected = state$regressor)
      }
    }
  ))
})
}


covariate_model_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center", class = "covariate_model_div",
      p("Model fit for all studies:"),
      tableOutput(ns("table"))
  )
}

covariate_model_module_rmd <- function(common){ list(
  covariate_model_knit = !is.null(common$meta$covariate_model$used),
  # covariate_model_dataset = common$meta$covariate_model$dataset,
  covariate_model_covariate_value = common$meta$covariate_model$covariate_value,
  covariate_model_regressor = common$meta$covariate_model$regressor)
}

