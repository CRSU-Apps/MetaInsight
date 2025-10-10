freq_cinema_export_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Generate CINeMA Project", icon = icon("arrow-turn-down"))
  )
}

freq_cinema_export_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    shinyjs::hide(selector = ".freq_cinema_export_div")

    observeEvent(input$run, {
      # WARNING
      if (is.null(common$freq_sub)){
        common$logger |> writeLog(type = "error", "Please configure the analysis first in the Setup section")
        return()
      }
      
      # TRIGGER
      shinyjs::show(selector = ".freq_cinema_export_div")
      trigger("freq_cinema_export")
    })
      
    output$download_export <- downloadHandler(
      filename = "metainsight-cinema-export.cnm",
      content = function(file) {
        readr::write_file(
          readr::read_file(system.file("extdata", "diabetes_basic.cnm", package = "metainsight")),
          file
        )
      }
    )
  
  })
}


freq_cinema_export_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  div(
    class = "freq_cinema_export_div",
    downloadButton(ns('download_export'), "Download CINeMA Project")
  )
}

