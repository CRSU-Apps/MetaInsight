summary_char_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate table", icon = icon("arrow-turn-down")),
    conditionalPanel("input.run > 0", downloadButton(ns("download")), ns = ns)
  )
}

summary_char_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$bugsnet_sub)){
        common$logger |> writeLog(type = "error", "Please configure the analysis first in the Setup section")
        return()
      }
      # TRIGGER
      trigger("summary_char")
    })

    summary_all <- reactive({
      req(watch("summary_char") > 0)
      common$meta$summary_char$used <- TRUE
      summary_char(common$bugsnet_all, common$outcome)
    })

    summary_sub <- reactive({
      watch("setup_exclude")
      req(watch("summary_char") > 0)
      summary_char(common$bugsnet_sub, common$outcome)
    })

    table <- reactive({
      df <- data.frame(summary_all()$Value, summary_sub()$Value)
      colnames(df) <- c("All studies", "With selected studies excluded")
      rownames(df) <- summary_all()$Characteristic
      df
    })

    # Characteristics table of all studies
    output$table <- renderTable({
      table()
    }, rownames = TRUE)

    output$download <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_study_characteristics.csv")
      },
      content = function(file) {
        write.csv(table(), file)
      }
    )

})
}

summary_char_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    div(style = "display: flex; justify-content: center; padding-top: 50px", tableOutput(ns("table")))
  )
}

summary_char_module_rmd <- function(common) {
  list(summary_char_knit = !is.null(common$meta$summary_char$used))
}

