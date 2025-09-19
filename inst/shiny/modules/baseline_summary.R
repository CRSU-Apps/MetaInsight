baseline_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module baseline_summary", icon = icon("arrow-turn-down"))

  )
}

baseline_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_summary")


  })

  output$result <- renderText({
    watch("baseline_summary")
    # Result
  })



})
}


baseline_summary_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


baseline_summary_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

