baseline_details_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module baseline_details", icon = icon("arrow-turn-down"))

  )
}

baseline_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_details")


  })

  output$result <- renderText({
    watch("baseline_details")
    # Result
  })



})
}


baseline_details_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


baseline_details_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

