baseline_deviance_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module baseline_deviance", icon = icon("arrow-turn-down"))

  )
}

baseline_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_deviance")


  })

  output$result <- renderText({
    watch("baseline_deviance")
    # Result
  })



})
}


baseline_deviance_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


baseline_deviance_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

