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

    # Characteristics table of all studies
    output$table <- renderTable({
      req(summary_all())
      df <- data.frame(summary_all()$Value, summary_sub()$Value)
      colnames(df) <- c("All studies", "With selected studies excluded")
      rownames(df) <- summary_all()$Characteristic
      df
    }, rownames = TRUE)

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

