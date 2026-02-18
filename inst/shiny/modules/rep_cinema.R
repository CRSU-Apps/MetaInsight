rep_cinema_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI

    radioButtons(ns("select_model"), "Select model to export", choices = list("Frequentist" = "freq", "Bayesian" = "bayes")),
    radioButtons(ns("select_data"), "Select dataset", choices = list("Full" = "full", "Sensitivity" = "sensitivity")),
    actionButton(ns("run"), "Create CINeMA export file", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download")

  )
}

rep_cinema_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide("download")

  observeEvent(input$run, {

    if (is.null(common$configured_data$freq)) {
      common$logger |> writeLog(type = "error", go_to = "setup_configure", "Please configure the analysis first")
      shinyjs::hide("download")
      return()
    }
    
    if (input$select_model == "bayes" && is.null(common$bayes_all)) {
      common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian model first")
      shinyjs::hide("download")
      return()
    }
    
    trigger("rep_cinema")
      
    shinyjs::show("download")

  })

  observe({
    if (input$select_model == "bayes" && is.null(common$bayes_all)) {
      shinyjs::hide("download")
    }
  })

  gemtc_results <- reactive({
    if (input$select_model == "freq") {
      return(NULL)
    } else if (input$select_model == "bayes") {
      if (input$select_data == "full") {
        return(common$bayes_all$mtcResults)
      } else if (input$select_data == "sensitivity") {
        return(common$bayes_sub$mtcResults)
      }
    }
  })
  
  selected_data <- reactive({
    if (input$select_data == "full") {
      return(common$configured_data) 
    } else {
      return(common$subsetted_data) 
    }
  })
  
  contributions <- reactive({
    netmeta::netcontrib(
      x = selected_data()$freq$net1,
      method = "shortestpath",
      study = TRUE
    )
  })
  
  cinema <- reactive({
    rep_cinema(
      data = selected_data()$connected_data,
      treatment_ids = selected_data()$treatments,
      outcome_type = selected_data()$outcome,
      contributions = contributions(),
      model_type = selected_data()$effects,
      outcome_measure = selected_data()$outcome_measure,
      gemtc_results = gemtc_results()
    )
  })
  

  output$download <- downloadHandler(
    filename = function() {
      paste0("cinema_", input$select_model, ".json")
    },
    content = function(file) {
      writeLines(cinema(), file)
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


