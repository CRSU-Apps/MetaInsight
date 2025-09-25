baseline_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down"))
  )
}

baseline_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_forest")
  })



  output$plot <- renderUI({
    watch("baseline_forest")
  })



})
}


baseline_forest_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  uiOutput(ns("plot"))
}


baseline_forest_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

