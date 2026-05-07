export_cinema_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("model"), "Select model to export", choices = list("Frequentist" = "freq", "Bayesian" = "bayes")),
    radioButtons(ns("data"), "Select dataset", choices = list("All studies" = "configured_data", "With selected studies excluded" = "subsetted_data")),
    actionButton(ns("run"), "Create CINeMA export file", icon = icon("arrow-turn-down")),
    div(style = "visibility: hidden;",
      downloadButton(ns("download"), "")
    )
  )
}

export_cinema_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {

    if (is.null(common$configured_data$freq)) {
      common$logger |> writeLog(type = "error", go_to = "setup_configure", "Please configure the analysis first")
      return()
    }

    if (!all(c("rob", "indirectness") %in% names(common$configured_data$connected_data))){
      common$logger |> writeLog(type = "error", "The uploaded data must contain 'rob' and 'indirectness' columns")
      return()
    }

    if (input$model == "bayes" && is.null(common$bayes_model_all)) {
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
          "configured_data" = "bayes_model_all",
          "subsetted_data" = "bayes_model_sub"
        )
      )
    }
  })

  output$download <- downloadHandler(
    filename = function() {
      dataset <- ifelse(input$data == "configured_data", "_all", "_sub")
      paste0("cinema_", input$model, dataset, ".cnm")
    },
    content = function(file) {
      writeLines(
        export_cinema(
          configured_data = common[[input$data]],
          gemtc_results = common[[gemtc_results()]]$mtcResults
        ),
        file
      )
    })

  return(list(
    save = function() {
      list(
        ### Manual save start
        ### Manual save end
        model = input$model,
        data = input$data
      )
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateRadioButtons(session, "model", selected = state$model)
      updateRadioButtons(session, "data", selected = state$data)
    }
  ))

})
}


