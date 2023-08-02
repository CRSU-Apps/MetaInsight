
#' Module ui for uploading data into the app.
#' 
#' @param id ID of the module
#' @return Div containing data upload controls
data_input_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    h4(tags$strong("Select a data file (.csv) to upload")),
    p(tags$strong("Files used before version 5.0 are no longer compatible. To use an older file, replace the numeric treatment IDs in the \"T\" column(s) with the treatment names.",
                  style = "color:red")),
    p(tags$strong("Default maximum file size is 5MB.")),
    uiOutput(outputId = ns("file_input_panel")),
    conditionalPanel(
      condition = 'output.data_uploaded == true',
      ns = ns,
      div(
        style = "float:right",
        actionButton(inputId = ns("reload_button"),
                     label = "Delete Data",
                     incon = icon("trash"),
                     style = "color: #fff; background-color: #dc3545; border-color: #dc3545")
      )
    ),
    div(class = "clearfix"),
    selectizeInput(inputId = ns('reference_treatment'), label = 'Select Reference Treatment', choices = c())
  )
}

#' Module server for uploading data into the app.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
#' @param continuous_file Defaut data file for continuous outcomes. Defaults to 'Cont_long.csv'
#' @param binary_file Defaut data file for binary outcomes. Defaults to 'Binary_long.csv'
#' @return List of reactives:
#'   - 'data' is the uplodaded data
#'   - 'treatment_list' is the data frame containing the treatment ID ('Number') and the treatment name ('Label')
data_input_panel_server <- function(id, metaoutcome, continuous_file = 'Cont_long.csv', binary_file = 'Binary_long.csv') {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a definable reactive value to allow reloading of data
    reload <- reactiveVal(FALSE)

    # Render function for file input dynamically to allow the button to be set to Null
    default_file_input <- renderUI({
      fileInput(
        inputId = ns("data"),
        label = "",
        buttonLabel = "Select",
        placeholder = "No file selected",
        accept = '.csv'
      )
    })

    # Logical to show reset button only when data uploaded
    data_uploaded <- reactiveVal(FALSE)
    output$data_uploaded <- reactive({data_uploaded()})
    outputOptions(output, 'data_uploaded', suspendWhenHidden = FALSE)

    # Render the file input intially
    output$file_input_panel <- default_file_input
    
    # Load default data
    defaultD <- reactive({
      if (metaoutcome() == 'Continuous') {
        defaultD <- read.csv(continuous_file)
      } else {
        defaultD <- read.csv(binary_file)
      }
    })
    
    # Make data reactive i.e. default or user uploaded
    data <- reactive({ 
      file1 <- input$data # Name the data file that was uploaded file1
      # if a reload is triggered show the reload the file input and data
      if (reload()) {
        output$file_input_panel <- default_file_input
        df <- defaultD()
      } else if (is.null(file1)) {
        # if data is triggered without reload, only load the default data
        df <- defaultD()
      } else {
        df <- read.table(file = file1$datapath,
                         sep = ",",
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         quote = "\"",
                         fileEncoding = 'UTF-8-BOM')
      }
      
      return(CleanData(df))
    })
    
    all_treatments <- reactive({
      return(FindAllTreatments(data()))
    })
    
    # Create the treatment list with the reference treatment being the first item in the data frame
    treatment_list <- reactive({
      return(CreateTreatmentIds(all_treatments(), input$reference_treatment))
    })
    
    
    #####
    # observer functions to trigger specific reactions
    #####
    
    # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
    observeEvent(metaoutcome(),
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   data_uploaded(FALSE)
                 })
    
    # if the data is changed load the new data and show the reload button
    observeEvent(input$data,
                 {
                   reload(FALSE)
                   data_uploaded(TRUE)
                 })
    
    # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
    observeEvent(input$reload_button,
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   data_uploaded(FALSE)
                 })
    
    # Reset the reference treatment when the data changes, by scanning through the uploaded data
    observe({
              treatments = all_treatments()
              updateSelectInput(inputId = 'reference_treatment',
                                choices = treatments,
                                selected = FindExpectedReferenceTreatment(treatments))
            })
    
    return(list(data = data, treatment_list = treatment_list))
  })
}