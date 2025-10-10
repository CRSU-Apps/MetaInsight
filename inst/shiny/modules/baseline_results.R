baseline_results_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module baseline_results", icon = icon("arrow-turn-down"))

  )
}

baseline_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_results")


  })

  output$result <- renderText({
    watch("baseline_results")
    # Result
  })



})
}


baseline_results_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


baseline_results_module_rmd <- function(common) {
  list(baseline_results_knit = !is.null(common$meta$baseline_results$used))
}

