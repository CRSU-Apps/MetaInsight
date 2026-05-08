freq_inconsistency_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down")),
    div(class = "freq_inconsistency", download_button_pair(id))
  )
}

freq_inconsistency_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id)

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$configured_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis first in the Setup section")
        return()
      }
      # TRIGGER
      trigger("freq_inconsistency")
    })

    table_all <- reactive({
      watch("freq_all")
      req(watch("freq_inconsistency") > 0)
      common$meta$freq_inconsistency$used <- TRUE
      freq_inconsistency(common$configured_data)
    })

    table_sub <- reactive({
      watch("setup_exclude")
      req(watch("freq_inconsistency") > 0)
      freq_inconsistency(common$subsetted_data)
    })

    output$table_all <- renderTable(colnames = TRUE, table_all())
    output$table_sub <- renderTable(colnames = TRUE, table_sub())

    outputOptions(output, "table_all", suspendWhenHidden = FALSE)

    output$download_all <- downloadHandler(
      filename = "MetaInsight_inconsistency_all.csv",
      content = function(file) {
        write.csv(table_all(), file)
      }
    )

    output$download_sub <- downloadHandler(
      filename = "MetaInsight_inconsistency_sub.csv",
      content = function(file) {
        write.csv(table_sub(), file)
      }
    )

})
}

freq_inconsistency_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    h4(class = "freq_inconsistency",
       "Assessment of inconsistency for all studies"),
    tableOutput(ns("table_all")),
    br(),
    h4(class = "freq_inconsistency",
       "Assessment of inconsistency with selected studies excluded"),
    tableOutput(ns("table_sub"))
  )
}

freq_inconsistency_module_rmd <- function(common) {
  list(freq_inconsistency_knit = !is.null(common$meta$freq_inconsistency$used))
}

