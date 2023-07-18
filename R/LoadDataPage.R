load_data_page_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Load Data",
    htmlOutput(outputId = ns("CONBI")),
    tags$head(tags$style(paste0("#", ns("CONBI"), "{",
                                "color: white;
                                font-size: 20px;
                                font-style: bold;
                                background-color: #2196c4
                                }")
    )),
    br(),
    sidebarLayout(
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
        div(style = "display:inline-block; float:right", actionButton("reload_labels", "Reload Default Labels", icon("arrows-rotate"), 
                                                                      style="color: #fff; background-color: #007bff; border-color: #007bff")),
        div(class = "clearfix")
      ),
      mainPanel(
        tabsetPanel(id = "instructions",
                    tabPanel("Long format upload", 
                             h2(tags$strong("Instructions for uploading long format data")),
                             br(),
                             p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for long format data, where each row contains one treatment arm. 
                          Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                          Instructions are as below.
                          Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment.")),
                             h4(tags$strong("Step 1:")),
                             p(),
                             conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                                              p("The long format data file should contain six columns. Headings of columns are case sensitive."), 
                                              p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
                                              p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                                              p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                                                                tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
                                              p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
                                              p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("Mean"), "and contain the mean value of the outcome in each arm of the study."))),
                                              p(tags$ul(tags$li("The", tags$strong("sixth"), "column should be labelled", tags$strong("SD"), "and contain the standard deviation of the outcome in each arm of the study.")))
                             ), 
                             conditionalPanel(condition = "input.metaoutcome=='Binary'", 
                                              p("The long format data file should contain five columns. Headings of columns are case sensitive."), 
                                              p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
                                              p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                                              p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                                                                tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
                                              p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("R"), 
                                                                "and contain the number of participants with the outcome of interest in each arm of the study."))),
                                              p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
                                              p("N.B. Continuity corrections will need to be applied to cells containing 0 values")               
                             ),
                             p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
                             p("The csv file that is used to produce the example dataset can be downloaded from here:"),
                             downloadButton(outputId = ns("downloadData"),
                                            label = "Download the example dataset in long format"),
                             br(),
                             h4(tags$strong("Step 2:")),
                             p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
                             p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
                             p("The default 'treatment labels' text file can be downloaded from here:"),
                             downloadButton(outputId = ns("downloadlabel"),
                                            label = "Download the example 'treatment labels' text file"),
                             br(),
                             p(),
                             conditionalPanel(condition = "input.metaoutcome=='Continuous'", 
                                              p(HTML(paste0("This default dataset for continuous outcome data is from Gray, LJ. et al. A systematic review and mixed treatment 
                comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.
                The continuous outcome used is BMI loss (kg/m",tags$sup("2"),") 3 months from baseline.")))
                             ),
                             conditionalPanel(condition = "input.metaoutcome=='Binary'", 
                                              p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                Medical Decision Making, 18, 37-43.
                The binary outcome used is smoking cessation.")
                             ),
                             br(),
                             p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data' tab will automatically update once a file is successfully loaded."))
                    ),
                    
                    tabPanel("Wide format upload",
                             h2(tags$strong("Instructions for uploading wide format data")),
                             br(),
                             p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for wide format data, where each row contains all the treatment arms from one study. Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                          Instructions are as below.
                          Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment.")),
                             h4(tags$strong("Step 1:")),
                             downloadButton(outputId = ns("downloadDataWide"),
                                            label = "Download the example dataset in wide format"),
                             br(),
                             p("Your data needs to have exactly the same variable names as in the example data which can be downloaded from here:"),
                             p("Headings of columns are case sensitive."),
                             p(tags$ul(tags$li(tags$strong("StudyID"), "contains study identifier, starting from 1, then 2, 3, 4... etc."))),
                             p(tags$ul(tags$li(tags$strong("Study"), "contains name (e.g., author,year) of the study. The study name must be unique for each study."))),
                             p(tags$ul(tags$li(tags$strong("T.1, T.2, ..., up to T.6"), "contains treatment given for study arm 1, 2, ..., up to 6, respectively given as a numerical code"))),
                             conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                                              p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively"))),
                                              p(tags$ul(tags$li(tags$strong("Mean.1, Mean.2, ..., up to Mean.6"), "contains the mean value of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
                                              p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.6"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively")))
                             ),
                             conditionalPanel(condition = "input.metaoutcome=='Binary'",
                                              p(tags$ul(tags$li(tags$strong("R.1, R.2, ..., up to R.6"), "contains number of participants with the outcome of interest in study arm 1, 2, ..., up to 6, respectively"))),
                                              p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively")))
                             ),
                             p(tags$strong("Note: If applicable, your reference treatment (e.g. Placebo/Control)", 
                                           tags$u("needs to be labelled as treatment 1"))),
                             p(tags$strong("The maximum number of arms for each trial allowed in the MetaInsight app is 6.")),
                             br(),
                             h4(tags$strong("Step 2:")),
                             p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
                             p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
                             p("The default 'treatment labels' text file can be downloaded from here:"),
                             downloadButton(outputId = ns("downloadlabel2"),
                                            label = "Download the example 'treatment labels' text file"),
                             br(),
                             p(),
                             conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                                              p("This default dataset is from Gray, LJ. et al. A systematic review and mixed treatment 
              comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.")
                             ),
                             conditionalPanel(condition = "input.metaoutcome=='Binary'",
                                              p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                Medical Decision Making, 18, 37-43.")
                             ),
                             br(),
                             p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels 
                          will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data'
                          tab will automatically update once a file is successfully loaded."))
                    ),
                    tabPanel("View Data", 
                             p("Please double check if the total number of treatments matches the total number of treatment labels, 
                    i.e. make sure each treatment code in the data has a corresponding treatment label, 
                    and there is no additional treatment label which does not exist in the data."),
                             uiOutput(outputId = ns("tb")))
        ))))
}

load_data_page_server <- function(id, metaoutcome) {
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
        actionButton(inputId = "reload_button",
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
    output$file_input = default_file_input
    
    
    # Load default data
    defaultD <- reactive({
      if (metaoutcome()=='Continuous') {
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
        output$file_input = default_file_input
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
        return (input$listCont)
      } else {
        return (input$listbina)
      }
    })
    
    
    #####
    # observer functions to trigger specific reactions
    #####
    
    # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
    observeEvent(metaoutcome(),
                 {
                   reload(TRUE)
                   output$file_input = default_file_input
                   output$reload_button = NULL
                 })
    
    # if the data is changed load the new data (reset the labels) and show the reload button
    observeEvent(input$data,
                 {
                   reload(FALSE)
                   output$reload_button = default_reload_button
                 })
    
    # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
    observeEvent(input$reload_button,
                 {
                   reload(TRUE)
                   output$file_input = default_file_input
                   output$reload_button = NULL
                 })
    
    # if the reload labels button is clicked reload the default labels
    observeEvent(input$reload_labels,
                 {
                   output$trt_panel <- renderUI({
                     default_trt_panel()
                   })
                 }, ignoreNULL = FALSE)
    
    # Allow the treatment list to be rendered without the data tab being loaded.
    outputOptions(x = output,
                  name = "trt_panel",
                  suspendWhenHidden = FALSE)
    
    ### Outcome selection
    output$CONBI <- renderText({
      paste("You have selected", "<font color=\"#ffd966\"><b>", metaoutcome(),"</b></font>", 
            "outcome on the 'Home' page. The instructions for formatting",
            "<font color=\"#ffd966\"><b>", metaoutcome(), "</b></font>", "outcomes are now displayed.")
    })
    
    ### Data analysis tab
    # Create a table which displays the raw data just uploaded by the user
    output$tb <- renderTable({
      if (is.null(data())) {
        return()
      }
      return(data())
    })
    
    ##### in the 'Upload long data' tab
    output$downloadData <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataLONG.csv", "Cont_long.csv", "Binary_long.csv")
    output$downloadlabel <- create_raw_data_download_handler(metaoutcome, "treatmentlabels.txt", "defaultlabels_continuous.txt", "defaultlabels_binary.txt")
    
    ##### in the 'Upload wide data' tab
    output$downloadDataWide <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataWIDE.csv", "Cont_wide.csv", "Binary_wide.csv")
    output$downloadlabel2 <- create_raw_data_download_handler(metaoutcome, "treatmentlabels.txt", "defaultlabels_continuous.txt", "defaultlabels_binary.txt")
    
    
    return(list(data = data, treatment_list = treatment_list))
  })
}