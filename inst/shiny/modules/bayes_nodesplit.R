bayes_nodesplit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module bayes_nodesplit", icon = icon("arrow-turn-down"))

  )
}

bayes_nodesplit_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("bayes_nodesplit")


  })

  output$result <- renderText({
    watch("bayes_nodesplit")
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


bayes_nodesplit_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


bayes_nodesplit_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

