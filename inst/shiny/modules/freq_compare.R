freq_compare_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module freq_compare")
  )
}

freq_compare_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    gargoyle::trigger("freq_compare")
  })

  output$result <- renderText({
    gargoyle::watch("freq_compare")
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      # Populate using save_and_load()
    },
    load = function(state) {
      # Load
      # Populate using save_and_load()
    }
  ))
})
}

freq_compare_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

freq_compare_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

