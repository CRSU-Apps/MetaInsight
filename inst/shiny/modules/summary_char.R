summary_char_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
  )
}

summary_char_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    summary_all <- reactive({
      watch("setup_define")
      req(common$bugsnet_all)
      common$meta$summary_char$used <- TRUE
      summary_char(common$bugsnet_all, common$outcome)
    })

    summary_sub <- reactive({
      watch("summary_exclude")
      watch("setup_define")
      req(common$bugsnet_sub)
      summary_char(common$bugsnet_sub, common$outcome)
    })

    table <- reactive({
      req(summary_all())
      df <- data.frame(summary_all()$Value, summary_sub()$Value)
      colnames(df) <- c("All studies", "With selected studies excluded")
      rownames(df) <- summary_all()$Characteristic
      df
    })

    # Characteristics table of all studies
    output$table <- renderTable({
      watch("setup_define")
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
    div(style = "display: flex; justify-content: center; padding-top: 50px", tableOutput(ns("table"))),
    downloadButton(ns("download")) # this needs something to reduce the width (SS)
  )
}

summary_char_module_rmd <- function(common) {
  list(summary_char_knit = !is.null(common$meta$summary_char$used))
}

