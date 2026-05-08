setup_upgrade_module_ui <- function(id) {
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
    actionButton(ns("run"), "Update data", icon = icon("arrow-turn-down")),
    uiOutput(ns("download_out"))
  )
}

setup_upgrade_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$uploaded_data)) {
      common$logger |> writeLog(type = "error", "Please upload a file")
      return()
    }

    if (input$treatment_names == "") {
      common$logger |> writeLog(type = "error", "Please enter a list of treatment names")
      return()
    }

    # FUNCTION CALL ####
    result <- setup_upgrade(input$uploaded_data$datapath, input$treatment_names, common$logger)

    # LOAD INTO COMMON ####
    if (!is.null(result)){
      common$logger |> writeLog(type= "complete", "Data was upgraded successfully and can now be downloaded")
      common$upgraded_data <- result
      # METADATA ####
      common$meta$setup_upgrade$used <- TRUE
      common$meta$setup_upgrade$uploaded_data <- input$uploaded_data$name
      common$meta$setup_upgrade$treatment_names <- input$treatment_names

      # TRIGGER
      trigger("setup_upgrade")
    }
  })

  output$download_out <- renderUI({
    watch("setup_upgrade")
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
    save = function() {list(
      ### Manual save start
      ### Manual save end
      treatment_names = input$treatment_names)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateTextInput(session, "treatment_names", value = state$treatment_names)
    }
  ))
})
}



setup_upgrade_module_rmd <- function(common){ list(
  setup_upgrade_knit = !is.null(common$meta$setup_upgrade$used),
  setup_upgrade_uploaded_data = common$meta$setup_upgrade$uploaded_data,
  setup_upgrade_treatment_names = common$meta$setup_upgrade$treatment_names)
  # Variables used in the module's Rmd code
}

