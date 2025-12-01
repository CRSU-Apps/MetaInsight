# see covariate_deviance for module source
baseline_deviance_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

baseline_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_deviance_module_server("baseline", common, reactive(input$run))
  })
}

baseline_deviance_module_result <- function(id) {
  ns <- NS(id)
  metaregression_deviance_module_result(ns("baseline"), "bnma", "baseline_deviance_div")
}

baseline_deviance_module_rmd <- function(common) {
  list(baseline_deviance_knit = !is.null(common$meta$baseline_deviance$used))
}
