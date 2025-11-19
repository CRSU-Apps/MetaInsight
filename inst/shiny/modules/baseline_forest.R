baseline_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    metaregression_forest_module_ui(ns("baseline"), id)
  )
}

baseline_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_forest_module_server("baseline", common, reactive(input$run))
  })
}

baseline_forest_module_result <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_result(ns("baseline"))
}

baseline_forest_module_rmd <- function(common) {
  list(baseline_forest_knit = !is.null(common$meta$baseline_forest$used))
}

