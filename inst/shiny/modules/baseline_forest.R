baseline_forest_module_ui <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_ui(ns("baseline"))
}

baseline_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_forest_module_server("baseline", common)
})
}


baseline_forest_module_result <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_result(ns("baseline"))
}


baseline_forest_module_rmd <- function(common) {
  list(baseline_forest_knit = !is.null(common$meta$baseline_forest$used))
}

