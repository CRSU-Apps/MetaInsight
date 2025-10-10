covariate_comparison_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate table", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download table")
  )
}

covariate_comparison_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide("download")

    observeEvent(input$run, {
      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "error", "Please fit the covariate model first")
        return()
      } else {
        common$meta$covariate_comparison$used <- TRUE
        shinyjs::show("download")
        trigger("covariate_comparison")
      }
    })

    output$table <- renderTable({
      watch("covariate_model_fit")
      req(watch("covariate_comparison") > 0)
      covariate_comparison(common$covariate_model)
    })

    output$download <- downloadHandler(
      filename = function(){
        "MetaInsight_covariate_comparison.csv"
      },
      content = function(file) {
        write.csv(covariate_comparison(common$covariate_model), file)
      }
    )

})
}


covariate_comparison_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      tableOutput(ns("table"))
  )
}


covariate_comparison_module_rmd <- function(common) {
  list(covariate_comparison_knit = !is.null(common$meta$covariate_comparison$used))
}

