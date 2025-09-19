covariate_regression_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module covariate_regression", icon = icon("arrow-turn-down"))

  )
}

covariate_regression_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("covariate_regression")


  })

  output$result <- renderText({
    watch("covariate_regression")
    # Result
  })



})
}


covariate_regression_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


covariate_regression_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

