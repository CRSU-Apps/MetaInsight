covariate_results_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down"))
  )
}

covariate_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".covariate_results_div")

    observeEvent(input$run, {
      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "error", "Please fit the covariate model first")
        return()
      } else {
        trigger("covariate_results")
        common$meta$covariate_results$used <- TRUE
        shinyjs::show(selector = ".covariate_results_div")
      }
    })

    all_trigger <- reactive({
      if (watch("covariate_results") > 0){
        return(list(watch("covariate_results"), watch("covariate_model_fit")))
      }
    })

    bayes_results_submodule_server("all", common, "covariate_model", all_trigger)

})
}


covariate_results_module_result <- function(id) {
  ns <- NS(id)
  bayes_results_submodule_result(ns("all"), "for covariate model", "covariate_results_div")
}


covariate_results_module_rmd <- function(common) {
  list(covariate_results_knit = !is.null(common$meta$covariate_results$used))
}

