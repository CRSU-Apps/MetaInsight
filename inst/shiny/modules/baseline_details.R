baseline_details_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate details", icon = icon("arrow-turn-down"))
  )
}

baseline_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    bayes_details_submodule_server("baseline", common, "baseline_model", "baseline_model_fit", "baseline_details", "baseline_deviance", "baseline_deviance",
                                   "Please fit the baseline models first", reactive(input$run))
})
}

baseline_details_module_result <- function(id) {
  ns <- NS(id)
  bayes_details_submodule_result(ns("baseline"), "baseline_details_div")
}

baseline_details_module_rmd <- function(common) {
  list(baseline_details_knit = !is.null(common$meta$baseline_details$used),
       baseline_deviance_knit = !is.null(common$meta$baseline_deviance$used))
}

