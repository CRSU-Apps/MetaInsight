
#' Module UI for the wide format data upload instructions.
#' 
#' @param id ID of the module
#' @return Tab panel containing data upload instructions and example data downloads
wide_format_upload_panel_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Wide format upload",
           h2(tags$strong("Instructions for uploading wide format data")),
           br(),
           p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for wide format data, where each row contains all the treatment arms from one study. 
                          Instructions are as below.
                          Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment.")),
           h4(tags$strong("Step 1:")),
           p("The wide format data file should contain the following columns:"),
           p(tags$ul(tags$li(tags$strong("Study"), "contains name (e.g., author,year) of the study. The study name must be unique for each study."))),
           p(tags$ul(tags$li(tags$strong("T.1, T.2, ..., up to T.6"), "contains name of the treatment given for study arm 1, 2, ..., up to 6, respectively. Treatment names should only contain letters, numbers and underscores."))),
           conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                            p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively"))),
                            p(tags$ul(tags$li(tags$strong("Mean.1, Mean.2, ..., up to Mean.6"), "contains the mean value of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
                            p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.6"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively")))),
           conditionalPanel(condition = "input.metaoutcome=='Binary'",
                            p(tags$ul(tags$li(tags$strong("R.1, R.2, ..., up to R.6"), "contains number of participants with the outcome of interest in study arm 1, 2, ..., up to 6, respectively"))),
                            p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively")))),
           p(tags$strong("Headings of columns are case sensitive.")),
           p(tags$strong("The maximum number of arms for each trial allowed in the MetaInsight app is 6.")),
           p("Your data needs to have exactly the same variable names as in the example data which can be downloaded from here:"),
           downloadButton(outputId = ns("downloadDataWide"),
                          label = "Download the example dataset in wide format"),
           br(),
           h4(tags$strong("Step 2:")),
           p('Select the reference treatment in the drop-down box. By default, MetaInsight will select a treatment which matches common names for a standard reference treatment.'),
           hr(),
           conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                            p("This default dataset is from Gray, LJ. et al. A systematic review and mixed treatment 
                               comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.")),
           conditionalPanel(condition = "input.metaoutcome=='Binary'",
                            p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                               Medical Decision Making, 18, 37-43.")),
           br(),
           p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab,
                          will be used for analysis if no file is selected. The 'View Data'
                          tab will automatically update once a file is successfully loaded."))
  )
}

#' Module server for the wide format data upload instructions.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
wide_format_upload_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    output$downloadDataWide <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataWIDE.csv", "Cont_wide.csv", "Binary_wide.csv")
  })
}