baseline_comparison_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module baseline_comparison", icon = icon("arrow-turn-down"))

  )
}

baseline_comparison_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("baseline_comparison")


  })

  output$result <- renderText({
    watch("baseline_comparison")
    # Result
  })



})
}


baseline_comparison_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


baseline_comparison_module_rmd <- function(common) {
  list(baseline_comparison_knit = !is.null(common$meta$baseline_comparison$used))
}

