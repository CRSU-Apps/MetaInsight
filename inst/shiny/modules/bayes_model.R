bayes_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Fit models", type = "default", icon = icon("arrow-turn-down")),
    div(class = "bayes_model_div download_buttons",
        actionButton(ns("run_all"), "Run all modules", icon = icon("forward-fast"))
    )
  )
}

bayes_model_submodule_server <- function(id, common){
  moduleServer(id, function(input, output, session) {

    output$table <- renderTable({
      watch("bayes_model") # required for reset
      watch(paste0("bayes_model_table_", id))
      req(common[[paste0("bayes_", id)]])
      shinyjs::show(selector = ".bayes_model_div")
      common[[paste0("bayes_", id)]]$dic
    }, digits = 3, rownames = TRUE, colnames = FALSE)

    outputOptions(output, "table", suspendWhenHidden = FALSE)

  })
}

bayes_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # used to trigger downstream actions when model is rerun
    init("bayes_model_sub")
    init("bayes_model_all")

    # used to trigger summary tables - needs to be separate to reload
    init("bayes_model_table_sub")
    init("bayes_model_table_all")

    hide_and_show(id, show = FALSE)

    observeEvent(input$run, {
      if (is.null(common$configured_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis in the Setup component first.")
        return()
      }
      if (common$configured_data$outcome_measure == "SMD") {
        common$logger |> writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
        return()
      }
      else if (common$configured_data$outcome_measure == "RD") {
        common$logger |> writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
        return()
      }
      # METADATA ####
      common$meta$bayes_model$used <- TRUE
      trigger("bayes_model")
    })

    common$tasks$bayes_model_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_model, .args = environment())
    ) |> bind_task_button("run")

    # needed to cancel in progress
    sub_model <- NULL
    common$tasks$bayes_model_sub <- ExtendedTask$new(
      function(...) sub_model <<- mirai::mirai(run(...), run = bayes_model, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("bayes_model"), watch("setup_configure"), watch("effects")), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch("bayes_model") > 0 || all(!is.null(common$bayes_all), watch("effects") > 0)))

      # prevent both models fitting at once if the data is large
      if (nrow(common$configured_data$treatments) > 20){
        mirai::daemons(0)
        mirai::daemons(1)
      }

      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "starting", "Fitting Bayesian models")
      } else {
        common$logger |> writeLog(type = "starting", "Updating Bayesian model for main analysis")
      }
      common$tasks$bayes_model_all$invoke(common$configured_data, async = TRUE)

      result_all$resume()
    })

    observeEvent(list(watch("bayes_model"), watch("setup_exclude")), {
      # listen to both once they are run
      req((watch("bayes_model") + watch("setup_exclude")) > 0)
      # stop setup_exclude from triggering prior to bayes_model being run,
      # but enable it to run after reloading if studies are excluded
      req(common$meta$bayes_model$used)

      # cancel if the model is already updating
      if (common$tasks$bayes_model_sub$status() == "running"){
        mirai::stop_mirai(sub_model)
      }

      # prevent showing on first run
      if (!is.null(common$bayes_sub)){
        common$logger |> writeLog(type = "starting", "Updating Bayesian model for sensitivity analysis")
      }

      common$tasks$bayes_model_sub$invoke(common$subsetted_data, async = TRUE)
      result_sub$resume()
    })


    result_all <- observe({

      result <- common$tasks$bayes_model_all$result()
      result_all$suspend()
      if (inherits(result, "bayes_model")){
        common$bayes_all <- result
        shinyjs::runjs("Shiny.setInputValue('bayes_model-all-complete', 'complete');")
        common$logger |> writeLog(type = "complete", "Bayesian models have been fitted")
      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger("bayes_model_all")
      trigger("bayes_model_table_all")
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$bayes_model_sub$status() == "success"){
        result <- common$tasks$bayes_model_sub$result()
        result_sub$suspend()
        if (inherits(result, "bayes_model")){
          # prevent showing on first run
          if (!is.null(common$bayes_sub)){
            shinyjs::runjs("Shiny.setInputValue('bayes_model-sub-updated', 'updated');")
            common$logger |> writeLog(type = "complete", "The Bayesian model for the sensitivity analysis has been updated")
          }
          common$bayes_sub <- result
          shinyjs::runjs("Shiny.setInputValue('bayes_model-sub-complete', 'complete');")
        } else {
          common$logger |> writeLog(type = "error", result)
        }
        # reset daemons
        if (nrow(common$configured_data$treatments) > 20){
          mirai::daemons(0)
          mirai::daemons(4)
        }

        trigger("bayes_model_sub")
        trigger("bayes_model_table_sub")
      }
    })

    bayes_model_submodule_server("all", common)
    bayes_model_submodule_server("sub", common)

    observeEvent(input$run_all, {
      run_all(COMPONENTS, COMPONENT_MODULES, "bayes", common$logger)
    })

})
}

bayes_model_submodule_result <- function(id, label) {
  ns <- NS(id)
  tagList(
    p(paste0("Model fit ", label, ":")),
    tableOutput(ns("table"))
  )
}

bayes_model_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "bayes_model_div",
      fluidRow(
        column(
          width = 6,
          align = "center",
          bayes_model_submodule_result(ns("all"), "for all studies")
        ),
        column(
          width = 6,
          align = "center",
          bayes_model_submodule_result(ns("sub"), "excluding selected studies")
        )
      )
    )
  )
}

bayes_model_module_rmd <- function(common) {
  list(bayes_model_knit = !is.null(common$bayes_all))
}

