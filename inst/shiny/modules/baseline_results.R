baseline_results_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down"))
  )
}

baseline_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".baseline_results_div")

    observeEvent(input$run, {
      if (is.null(common$baseline_model)){
        common$logger |> writeLog(type = "error", "Please fit the baseline model first")
        return()
      } else {
        trigger("baseline_results")
        common$meta$baseline_results$used <- TRUE
        shinyjs::show(selector = ".baseline_results_div")
      }
    })

    all_trigger <- reactive({
      if (watch("baseline_results") > 0){
        return(list(watch("baseline_results"), watch("baseline_model_fit")))
      }
    })

    bayes_results_submodule_server("all", common, "baseline_model", all_trigger)

  })
}


baseline_results_module_result <- function(id) {
  ns <- NS(id)
  bayes_results_submodule_result(ns("all"), "for baseline model", "baseline_results_div")
}


baseline_results_module_rmd <- function(common) {
  list(baseline_results_knit = !is.null(common$meta$baseline_results$used))
}

