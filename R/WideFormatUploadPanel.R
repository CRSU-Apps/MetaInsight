wide_format_upload_panel_ui <- function(id) {
  ns <- NS(id)
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
                            p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.6"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively")))),
           conditionalPanel(condition = "input.metaoutcome=='Binary'",
                            p(tags$ul(tags$li(tags$strong("R.1, R.2, ..., up to R.6"), "contains number of participants with the outcome of interest in study arm 1, 2, ..., up to 6, respectively"))),
                            p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively")))),
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
                               comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.")),
           conditionalPanel(condition = "input.metaoutcome=='Binary'",
                            p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                               Medical Decision Making, 18, 37-43.")),
           br(),
           p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels 
                          will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data'
                          tab will automatically update once a file is successfully loaded."))
  )
}

wide_format_upload_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    output$downloadData <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataLONG.csv", "Cont_long.csv", "Binary_long.csv")
    output$downloadlabel <- create_raw_data_download_handler(metaoutcome, "treatmentlabels.txt", "defaultlabels_continuous.txt", "defaultlabels_binary.txt")
  })
}