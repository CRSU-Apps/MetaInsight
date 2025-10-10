# see covariate_regression module for source

baseline_regression_module_ui <- function(id) {
  ns <- NS(id)
  metaregression_regression_module_ui(ns("baseline"))
}

baseline_regression_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_regression_module_server("baseline", common)
})
}

baseline_regression_module_result <- function(id) {
  ns <- NS(id)
  metaregression_regression_module_result(ns("baseline"))
}


baseline_regression_module_rmd <- function(common) {
  metaregression_regression_module_rmd(common, "baseline_regression")
}

