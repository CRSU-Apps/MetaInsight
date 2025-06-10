bayes_results_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI


    actionButton(ns("run"), "Run module bayes_results", icon = icon("arrow-turn-down"))

  )
}

bayes_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    trigger("bayes_results")


  })

  output$result <- renderText({
    watch("bayes_results")
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


bayes_results_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


bayes_results_module_rmd <- function(common) {
  list(bayes_results_knit = FALSE)
}

