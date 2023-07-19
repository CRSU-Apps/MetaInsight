data_input_panel_ui <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    h4(tags$strong("Select a data file (.csv) to upload")),
    p(tags$strong("Files used before version 5.0 are no longer compatible. To use an older file, replace the numeric treatment IDs in the \"T\" column(s) with the treatment names.",
                  style = "color:red")),
    p(tags$strong("Note: Excel files should be saved in 'csv (Comma delimited) (*.csv)' format. Default maximum file size is 5MB.")),
    uiOutput(outputId = ns("file_input_panel")),
    uiOutput(outputId = ns("reload_button_panel")),
    div(class = "clearfix")
  )
}

data_input_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Create a definable reactive value to allow reloading of data
    reload <- reactiveVal(FALSE)

    # Render function for file input dynamically to allow the button to be set to Null
    default_file_input <- renderUI({
      fileInput(
        inputId = ns("data"),
        label = "",
        buttonLabel = "Select",
        placeholder = "No file selected"
      )
    })

    # Render function reload button dynamically to allow the button to be set to Null
    default_reload_button <- renderUI({
      div(
        style = "display:inline-block; float:right",
        actionButton(inputId = ns("reload_button"),
                     label = "Delete Data",
                     incon = icon("trash"),
                     style = "color: #fff; background-color: #dc3545; border-color: #dc3545")
      )
    })

    # Render the file input intially
    output$file_input_panel <- default_file_input
    
    
    # Load default data
    defaultD <- reactive({
      if (metaoutcome() == 'Continuous') {
        defaultD <- read.csv("./Cont_long.csv")
      } else {
        defaultD <- read.csv("./Binary_long.csv")
      }
    })
    
    # Make data reactive i.e. default or user uploaded
    data <- reactive({ 
      file1 <- input$data # Name the data file that was uploaded file1
      # if a reload is triggered show the reload the file input and data
      if (reload()) {
        output$file_input_panel <- default_file_input
        return(defaultD())
      } else if (is.null(file1)) {
        # if data is triggered without reload, only load the default data
        return(defaultD())
      } else {
        return(read.table(file = file1$datapath,
                          sep = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          quote = "\"",
                          fileEncoding = 'UTF-8-BOM'))
      }
    })
    
    
    #####
    # observer functions to trigger specific reactions
    #####
    
    # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
    observeEvent(metaoutcome(),
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   output$reload_button_panel <- NULL
                 })
    
    # if the data is changed load the new data (reset the labels) and show the reload button
    observeEvent(input$data,
                 {
                   reload(FALSE)
                   output$reload_button_panel <- default_reload_button
                 })
    
    # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
    observeEvent(input$reload_button,
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   output$reload_button_panel <- NULL
                 })
    
    return(list(data = data))
  })
}