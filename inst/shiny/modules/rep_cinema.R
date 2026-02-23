rep_cinema_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI

    radioButtons(ns("model"), "Select model to export", choices = list("Frequentist" = "freq", "Bayesian" = "bayes")),
    radioButtons(ns("data"), "Select dataset", choices = list("All studies" = "configured_data", "With selected studies excluded" = "subsetted_data")),
    actionButton(ns("run"), "Create CINeMA export file", icon = icon("arrow-turn-down")),
    div(style = "visibility: hidden;",
      downloadButton(ns("download"), "")
    )
    
  )
}

rep_cinema_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide("download")

  observeEvent(input$run, {

    if (is.null(common$configured_data$freq)) {
      common$logger |> writeLog(type = "error", go_to = "setup_configure", "Please configure the analysis first")
      return()
    }
    
    if (input$model == "bayes" && is.null(common$bayes_all)) {
      common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian model first")
      return()
    }
    
    shinyjs::click("download")

  })
  
  gemtc_results <- reactive({
    if (input$model == "freq") {
      return("NULL")
    } else if (input$model == "bayes") {
      return(
        switch(
          input$data,
          "configured_data" = "bayes_all",
          "subsetted_data" = "bayes_sub"
        )
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("cinema_", input$model, ".json")
    },
    content = function(file) {
      writeLines(
        rep_cinema(
          configured_data = common[[input$data]],
          gemtc_results = common[[gemtc_results()]]$mtcResults
        ),
        file
      )
    })

  return(list(
    save = function() {
      shinyscholar::save_and_load(".", "rep_cinema")
    },
    load = function(state) {
      shinyscholar::save_and_load(".", "rep_cinema")
    }
  ))

})
}


