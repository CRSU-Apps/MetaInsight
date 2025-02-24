freq_inconsistent_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module freq_inconsistent")
  )
}

freq_inconsistent_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    gargoyle::trigger("freq_inconsistent")
  })

    table_all <- reactive({
      watch("setup_define")
      watch("model")
      req(common$freq_all)
      common$meta$freq_inconsistent$used <- TRUE
      freq_inconsistent(common$freq_all, common$model_type)
    })

    table_sub <- reactive({
      watch("summary_exclude")
      req(common$freq_sub)
      freq_inconsistent(common$freq_sub, common$model_type)
    })

    output$table_all <- renderTable(colnames=TRUE, table_all())
    output$table_sub <- renderTable(colnames=TRUE, table_sub())

    output$download_all <- downloadHandler(
      filename = "MetaInight_inconsistency_all.csv",
      content = function(file) {
        write.csv(table_all(), file)
      }
    )

    output$download_sub <- downloadHandler(
      filename = "MetaInight_inconsistency_sub.csv",
      content = function(file) {
        write.csv(table_sub(), file)
      }
    )

})
}

freq_inconsistent_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Assessment of inconsistency for all studies"),
    tableOutput(ns("table_all")),
    downloadButton(ns("download_all")),
    br(),
    h4("Assessment of inconsistency with selected studies excluded"),
    tableOutput(ns("table_sub")),
    downloadButton(ns("download_sub"))
  )
}

freq_inconsistent_module_rmd <- function(common) {
  list(freq_inconsistent_knit = !is.null(common$meta$freq_inconsistent$used))
}

