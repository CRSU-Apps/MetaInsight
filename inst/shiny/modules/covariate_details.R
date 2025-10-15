covariate_details_module_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("run"), "Generate details", icon = icon("arrow-turn-down"))
}

covariate_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    bayes_details_submodule_server("covariate", common, "covariate_model", "covariate_model_fit", "covariate_details", "covariate_deviance", "covariate_deviance",
                                   "Please fit the covariate models first", reactive(input$run))
  })
}

covariate_details_module_result <- function(id) {
  ns <- NS(id)
  bayes_details_submodule_result(ns("covariate"), "covariate_details_div")
}

covariate_details_module_rmd <- function(common) {
  list(covariate_details_knit = !is.null(common$meta$covariate_details$used),
       covariate_deviance_knit = !is.null(common$meta$covariate_deviance$used))
}

