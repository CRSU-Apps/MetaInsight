load_update_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module load_update")
  )
}

load_update_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("load_update")
  })

  output$result <- renderText({
    gargoyle::watch("load_update")
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

load_update_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

load_update_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

