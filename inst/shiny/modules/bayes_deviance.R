bayes_deviance_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

bayes_deviance_submodule_server <- function(id, common, trigger){
  moduleServer(id, function(input, output, session) {

    output$scat <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      shinyjs::show(selector = ".bayes_deviance_div")
      common[[paste0("bayes_deviance_", id)]]$scat_plot
    })

    output$stem <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      common[[paste0("bayes_deviance_", id)]]$stem_plot
    })

    output$lev <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      common[[paste0("bayes_deviance_", id)]]$lev_plot
    })

  })
}


bayes_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_deviance_all")
    init("bayes_deviance_sub")
    shinyjs::hide(selector = ".bayes_deviance_div")

    observeEvent(input$run, {
      # add check for a running model

      if (is.null(common$bayes_all)){
        common$logger %>% writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_deviance$used <- TRUE
        trigger("bayes_deviance")
      }
    })

    common$tasks$bayes_deviance_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_deviance, .args = environment())
    ) %>% bind_task_button("run")

    common$tasks$bayes_deviance_sub <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_deviance, .args = environment())
    ) %>% bind_task_button("run")

    observeEvent(list(watch("bayes_deviance"), watch("bayes_model_all")), {
      req(watch("bayes_deviance") > 0)
      common$logger %>% writeLog(type = "starting", "Generating Bayesian deviance plots")
      common$tasks$bayes_deviance_all$invoke(common$bayes_all,
                                             common$model_type,
                                             common$outcome_measure)
      result_all$resume()
    })

    observeEvent(list(watch("bayes_deviance"), watch("bayes_model_sub")), {
      req(watch("bayes_deviance") > 0)

      # prevent showing on first run
      if (!is.null(common$bayes_deviance_sub)){
        common$logger %>% writeLog(type = "starting", "Updating Bayesian deviance plots")
      }

      common$tasks$bayes_deviance_sub$invoke(common$bayes_sub,
                                             common$model_type,
                                             common$outcome_measure)
      result_sub$resume()
    })

    result_all <- observe({
      result <- common$tasks$bayes_deviance_all$result()
      result_all$suspend()
      common$bayes_deviance_all <- result
      common$logger %>% writeLog(type = "complete", "Bayesian deviance plots have been generated")
      trigger("bayes_deviance_all")
    })

    result_sub <- observe({
      result <- common$tasks$bayes_deviance_sub$result()
      result_sub$suspend()
        if (!is.null(common$bayes_deviance_sub)){
          common$logger %>% writeLog(type = "complete", "Bayesian deviance plots have been updated")
        }
      common$bayes_deviance_sub <- result
      trigger("bayes_deviance_sub")
    })

    bayes_deviance_submodule_server("all", common, "bayes_deviance_all")
    bayes_deviance_submodule_server("sub", common, "bayes_deviance_sub")

})
}

bayes_deviance_submodule_result <- function(id, label) {
  ns <- NS(id)
  tagList(
    div(class = "bayes_deviance_div",
        h5(glue::glue("Residual deviance from NMA model and UME inconsistency model {label}"))
    ),
    plotly::plotlyOutput(ns("scat")),
    div(class = "bayes_deviance_div",
        h5(glue::glue("Per-arm residual deviance {label}"))
    ),
    plotly::plotlyOutput(ns("stem")),
    div(class = "bayes_deviance_div",
        h5(glue::glue("Leverage plot {label}"))
    ),
    plotly::plotlyOutput(ns("lev"))
  )
}


bayes_deviance_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        align = "center",
        bayes_deviance_submodule_result(ns("all"), "for all studies")
      ),
      column(
        width = 6,
        align = "center",
        bayes_deviance_submodule_result(ns("sub"), "excluding selected studies")
      )
    )
  )
}

bayes_deviance_module_rmd <- function(common) {
  list(bayes_deviance_knit = !is.null(common$meta$bayes_deviance$used))
}
