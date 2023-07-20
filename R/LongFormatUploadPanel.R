
#' Module UI for the long format data upload instructions.
#' 
#' @param id ID of the module
#' @return Tab panel containing data upload instructions and example data downloads
long_format_upload_panel_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Long format upload", 
           h2(tags$strong("Instructions for uploading long format data")),
           br(),
           p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for long format data, where each row contains one treatment arm.
                          Instructions are as below.
                          Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment.")),
           p(),
           conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                            p("The long format data file should contain six columns. Headings of columns are case sensitive."), 
                            p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
                            p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                            p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the name of the treatment used in each arm of the study.", 
                                              tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be the first study arm listed."))))),
                            p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
                            p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("Mean"), "and contain the mean value of the outcome in each arm of the study."))),
                            p(tags$ul(tags$li("The", tags$strong("sixth"), "column should be labelled", tags$strong("SD"), "and contain the standard deviation of the outcome in each arm of the study.")))), 
           conditionalPanel(condition = "input.metaoutcome=='Binary'", 
                            p("The long format data file should contain five columns. Headings of columns are case sensitive."), 
                            p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
                            p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                            p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                                              tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
                            p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("R"), 
                                              "and contain the number of participants with the outcome of interest in each arm of the study."))),
                            p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
                            p("N.B. Continuity corrections will need to be applied to cells containing 0 values")),
           p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
           p("The csv file that is used to produce the example dataset can be downloaded from here:"),
           downloadButton(outputId = ns("download_long_data"),
                          label = "Download the example dataset in long format"),
           p(),
           conditionalPanel(condition = "input.metaoutcome=='Continuous'", 
                            p(HTML(paste0("This default dataset for continuous outcome data is from Gray, LJ. et al. A systematic review and mixed treatment 
                                           comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.
                                           The continuous outcome used is BMI loss (kg/m",tags$sup("2"),") 3 months from baseline.")))),
           conditionalPanel(condition = "input.metaoutcome=='Binary'", 
                            p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                               Medical Decision Making, 18, 37-43.
                               The binary outcome used is smoking cessation.")),
           br(),
           p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, will be used for analysis if no file is selected. The 'View Data' tab will automatically update once a file is successfully loaded."))
  )
}

#' Module server for the long format data upload instructions.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
long_format_upload_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    output$download_long_data <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataLONG.csv", "Cont_long.csv", "Binary_long.csv")
  })
}