load_update_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h4(tags$strong("Select a data file (.csv) to upload")),
    fileInput(ns("uploaded_data"),
      label = "",
      buttonLabel = "Select",
      placeholder = "No file selected",
      accept = ".csv"
    ),
    textInput(ns("treatment_names"),
      label = "Type in the treatment names in order, separated by commas. Eg. \"Placebo,Paracetamol,Ibuprofen\""
    ),
    actionButton(ns("run"), "Update data"),
    uiOutput(ns("download_out"))
  )
}

load_update_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$uploaded_data)) {
      common$logger %>% writeLog(type = "error", "Please upload a file")
      return()
    }

    if (input$treatment_names == "") {
      common$logger %>% writeLog(type = "error", "Please enter a list of treatment names")
      return()
    }

    if (!grepl("^[a-zA-Z_]+(,[a-zA-Z_]+)*$", input$treatment_names)){
      common$logger %>% writeLog(type = "error", "The treatment names must only contain words separated by commas")
    }

    # FUNCTION CALL ####
    result <- load_update(input$uploaded_data$datapath, input$treatment_names, common$logger)

    # LOAD INTO COMMON ####
    if (!is.null(result)){
      common$logger %>% writeLog(type= "complete", "Data was upgraded successfully and can now be downloaded")
      common$upgraded_data <- result
      # METADATA ####

      # TRIGGER
      gargoyle::trigger("load_update")
    }
  })

  output$download_out <- renderUI({
    gargoyle::watch("load_update")
    req(common$upgraded_data)
    downloadButton(session$ns("download"))
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("upgraded_", input$uploaded_data$name)
    },
    content = function(file) {
      write.csv(file = file, x = common$upgraded_data, row.names = FALSE, quote = FALSE)
    }
  )


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



load_update_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

