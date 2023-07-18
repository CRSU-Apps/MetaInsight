data_input_panel_ui <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    h4(tags$strong("Step 1 - Please select a data file (.csv) to upload")),
    br(),
    p(tags$strong("Note: Excel files should be saved in 'csv (Comma delimited) (*.csv)' format. Default maximum file size is 5MB.")),
    uiOutput(outputId = ns("file_input")),
    uiOutput(outputId = ns("reload_button")),
    br(),
    tags$hr(),
    h4(tags$strong("Step 2 - Please copy and paste the treatment labels")),
    br(),
    p(tags$strong("Note: The first row must be 'Number' tabspace 'Label' as shown in the pre-loaded format, case sensitive.")),
    p(tags$strong("      Treatment names may only contain letters, digits, and underscore (_).")),
    p(tags$strong("Tabspace does not work when directly typing into the texbox. Please copy and paste from a text or Excel file, or copy and paste from one of the pre-loaded rows.")),
    br(),
    uiOutput(outputId = ns("trt_panel")),
    div(style = "display:inline-block; float:right",
        actionButton(inputId = ns("reload_labels"),
                     label = "Reload Default Labels",
                     icon = icon("arrows-rotate"), 
                     style = "color: #fff; background-color: #007bff; border-color: #007bff")),
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

    # Make the treatment panel reactive to allow switching between continous and binary more dynamic
    default_trt_panel <- reactive({
      # respond to reload
      reload()
      if (metaoutcome() == 'Continuous') {
        return(
          panel(
            aceEditor(
              outputId = ns("listCont"),
              value = paste0(
                "Number\tLabel",
                "\n1\tPlacebo",
                "\n2\tOrlistat",
                "\n3\tSibutramine",
                "\n4\tMetformin",
                "\n5\tOrli_Sibut",
                "\n6\tRimonbant"),
              mode = "r" ,
              theme = "eclipse"
            )
          )
        )
      }
      else{
        return(
          panel(
            aceEditor(
              outputId = ns("listbina"),
              value = paste0(
                "Number\tLabel",
                "\n1\tNo_contact",
                "\n2\tSelf_help",
                "\n3\tIndividual_counselling",
                "\n4\tGroup_counselling"),
              mode = "r",
              theme = "eclipse"
            )
          )
        )
      }
    })

    # Render the above treatment panel
    output$trt_panel <- renderUI({
      default_trt_panel()
    })

    # Render the file input intially
    output$file_input <- default_file_input
    
    
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
        output$file_input <- default_file_input
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
    
    # Make reactive treatment input list selecting correct input 
    # depending on if outcome is continuous or binary - NVB
    
    treatment_list <- reactive({
      if (metaoutcome() == "Continuous") {
        return(input$listCont)
      } else {
        return(input$listbina)
      }
    })
    
    
    #####
    # observer functions to trigger specific reactions
    #####
    
    # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
    observeEvent(metaoutcome(),
                 {
                   reload(TRUE)
                   output$file_input <- default_file_input
                   output$reload_button <- NULL
                 })
    
    # if the data is changed load the new data (reset the labels) and show the reload button
    observeEvent(input$data,
                 {
                   reload(FALSE)
                   output$reload_button <- default_reload_button
                 })
    
    # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
    observeEvent(input$reload_button,
                 {
                   reload(TRUE)
                   output$file_input <- default_file_input
                   output$reload_button <- NULL
                 })
    
    # if the reload labels button is clicked reload the default labels
    observeEvent(input$reload_labels,
                 {
                   output$trt_panel <- renderUI({
                     default_trt_panel()
                   })
                 },
                 ignoreNULL = FALSE)
    
    # Allow the treatment list to be rendered without the data tab being loaded.
    outputOptions(x = output,
                  name = "trt_panel",
                  suspendWhenHidden = FALSE)
    
    return(list(data = data, treatment_list = treatment_list))
  })
}