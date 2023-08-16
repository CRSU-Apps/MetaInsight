
#' Module ui for upgrading data from old versions of the app.
#' 
#' @param id ID of the module
#' @return Collapsible panel containing data upload controls
data_upgrade_panel_ui <- function(id) {
  ns <- NS(id)
  
  shinyBS::bsCollapse(
    id = ns("upgrade_data_collapse"),
    shinyBS::bsCollapsePanel(
      title = "Click here to upgrade old data files",
      h4(tags$strong("Select a data file (.csv) to upload")),
      fileInput(
        inputId = ns("uploaded_data"),
        label = "",
        buttonLabel = "Select",
        placeholder = "No file selected"
      ),
      textInput(
        inputId = ns("treatment_names"),
        label = "Type in the treatment names in order, separated by commas. Eg. \"Placebo,Paracetamol,Ibuprofen\""
      ),
      span(
        textOutput(outputId = ns("treatment_validation")),
        style = "color: red"
      ),
      downloadButton(outputId = ns("upgrade_download"))
    )
  )
}

#' Module server for upgrading data from old versions of the app.
#' 
#' @param id ID of the module
data_upgrade_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Make data reactive i.e. default or user uploaded
    data_to_upgrade <- reactive({
      return(
        read.table(
          file = input$uploaded_data$datapath,
          sep = ",",
          header = TRUE,
          stringsAsFactors = FALSE,
          quote = "\"",
          fileEncoding = 'UTF-8-BOM'
        )
      )
    })
    
    treatments_df <- reactive({
      return(CreateTreatmentsDataFrame(input$treatment_names))
    })
    
    treatment_name_surplus <- reactive({
      if (is.null(input$uploaded_data)) {
        return(0)
      }
      
      input_treatment_name_count <- nrow(treatments_df())
      data_treatment_name_count <- max(FindAllTreatments(data_to_upgrade()))
      return(input_treatment_name_count - data_treatment_name_count)
    })
    
    output$treatment_validation <- renderText({
      name_surplus <- treatment_name_surplus()
      if (name_surplus < 0) {
        return("Not enough treaments specified")
      } else if (name_surplus > 0) {
        return("Too many treaments specified")
      }
      return("")
    })
    
    observe({
      if (is.null(input$uploaded_data) | treatment_name_surplus() != 0) {
        shinyjs::disable(id = "upgrade_download")
      } else {
        shinyjs::enable(id = "upgrade_download")
      }
    })
    
    output$upgrade_download <- downloadHandler(
      filename = function() {
        paste0('upgraded_', input$uploaded_data$name)
      },
      content = function(file) {
        upgraded <- UpgradeData(data_to_upgrade(), treatments_df())
        write.csv(file = file, x = upgraded, row.names = FALSE, quote = FALSE)
      }
    )
  })
}
