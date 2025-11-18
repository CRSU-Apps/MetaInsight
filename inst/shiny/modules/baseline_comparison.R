baseline_comparison_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate table", icon = icon("arrow-turn-down")),
    metaregression_comparison_module_ui(ns("baseline"), id)
  )
}

baseline_comparison_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_comparison_module_server("baseline", common, reactive(input$run))
})
}

baseline_comparison_module_result <- function(id) {
  ns <- NS(id)
  metaregression_comparison_module_result(ns("baseline"), "baseline_comparison_div")
}

baseline_comparison_module_rmd <- function(common) {
  list(baseline_comparison_knit = !is.null(common$meta$baseline_comparison$used))
}

