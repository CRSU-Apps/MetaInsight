bayes_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Run models", type = "default", icon = icon("arrow-turn-down"))
  )
}

bayes_model_submodule_server <- function(id, common, run){
  moduleServer(id, function(input, output, session) {

    output$table <- renderTable({
      watch(paste0("bayes_model_", id))
      req(common[[paste0("bayes_", id)]])
      common[[paste0("bayes_", id)]]$dic
    }, digits = 3, rownames = TRUE, colnames = FALSE)

  })
}

bayes_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_model_sub")
    init("bayes_model_all")

    shinyjs::hide(selector = ".bayes_model_div")

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", "Please configure the analysis in the Setup component first.")
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

    observeEvent(list(watch("bayes_model"), watch("model")), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch("bayes_model") > 0 || all(!is.null(common$bayes_all), watch("model") > 0)))
      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "starting", "Fitting Bayesian models")
      } else {
        common$logger |> writeLog(type = "starting", "Updating Bayesian model for main analysis")
      }
      common$tasks$bayes_model_all$invoke(common$main_connected_data,
                                          common$treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_all,
                                          async = TRUE)


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

      common$tasks$bayes_model_sub$invoke(common$subsetted_data,
                                          common$subsetted_treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_sub,
                                          async = TRUE)
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
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$bayes_model_sub$status() == "success"){
        result <- common$tasks$bayes_model_sub$result()
        result_sub$suspend()
        if (inherits(result, "bayes_model")){
          # prevent showing on first run
          if (!is.null(common$bayes_sub)){
            common$logger |> writeLog(type = "complete", "The Bayesian model for the sensitivity analysis has been updated")
          }
          common$bayes_sub <- result
          shinyjs::runjs("Shiny.setInputValue('bayes_model-sub-complete', 'complete');")
          shinyjs::show(selector = ".bayes_model_div")
        } else {
          common$logger |> writeLog(type = "error", result)
        }
        trigger("bayes_model_sub")
      }
    })

    bayes_model_submodule_server("all", common)
    bayes_model_submodule_server("sub", common)


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

