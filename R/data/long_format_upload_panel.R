
#' Module UI for the long format data upload instructions.
#' 
#' @param id ID of the module
#' @return Tab panel containing data upload instructions and example data downloads
long_format_upload_panel_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Long format upload", 
    h2(tags$strong("Instructions for uploading long format data")),
    br(),
    p(
      tags$strong(
        "MetaInsight allows data in either long format, or wide format. This tab provides instructions for long format data, where each row contains one treatment arm.
        Instructions are as below.
        Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment."
      )
    ),
    h4(tags$strong("Step 1:")),
    p(),
    conditionalPanel(
      condition = "output.metaoutcome == 'Continuous'",
      ns = ns,
      p("The long format data file should contain the following columns:"),
      tags$ul(
        tags$li(tags$strong("Study"), " contains the name (e.g., author,year) of the study. The study name must be unique for each study.")
      ),
      tags$ul(
        tags$li(tags$strong("T"), " contains the name of the treatment used in each arm of the study. Treatment names should only contain letters, numbers and underscores.")
      ),
      tags$ul(
        tags$li(tags$strong("N"), " contains the number of participants in each arm of the study.")
      ),
      tags$ul(
        tags$li(tags$strong("Mean"), " contains the mean value of the outcome in each arm of the study.")
      ),
      tags$ul(
        tags$li(tags$strong("SD"), " contains the standard deviation of the outcome in each arm of the study.")
      )
    ), 
    conditionalPanel(
      condition = "output.metaoutcome == 'Binary'",
      ns = ns,
      p("The long format data file should contain the following columns:"),
      tags$ul(
        tags$li(tags$strong("Study"), " contains the name (e.g., author,year) of the study. The study name must be unique for each study.")
      ),
      tags$ul(
        tags$li(tags$strong("T"), " contains the name of the treatment used in each arm of the study. Treatment names should only contain letters, numbers and underscores.")
      ),
      tags$ul(
        tags$li(tags$strong("R"), " contains the number of participants with the outcome of interest in each arm of the study.")
      ),
      tags$ul(
        tags$li(tags$strong("N"), " contains the number of participants in each arm of the study.")
      ),
      p("N.B. Continuity corrections will need to be applied to cells containing 0 values")
    ),
    p("The long format data file may also contain the following column:"),
    tags$ul(
      tags$li(
        tags$strong("covar.<COVARIATE_NAME>"),
        " contains the study-level covariate value, where <COVARIATE_NAME> is replaced by the name of the covariate. This must be identical for each arm of the study. The name of the covariate will be extracted and used in the analysis output."
      )
    ),
    p(tags$strong("The maximum number of arms for each trial allowed in the MetaInsight app is 6.")),
    p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
    p("The csv file that is used to produce the example dataset can be downloaded from here:"),
    downloadButton(
      outputId = ns("download_long_data"),
      label = "Download the example dataset in long format"
    ),
    br(),
    h4(tags$strong("Step 2:")),
    p('Select the reference treatment in the drop-down box. By default, MetaInsight will select a treatment which matches common names for a standard reference treatment.'),
    hr(),
    conditionalPanel(
      condition = "output.metaoutcome=='Continuous'",
      ns = ns,
      p("This default dataset for continuous outcome data is a reduced version of the data from Brett Doleman, Ole Mathiesen, Alex J Sutton, Nicola J Cooper, Jon N Lund, John P Williams (2023),", tags$em("Non-opioid analgesics for the prevention of chronic postsurgical pain: a systematic review and network meta-analysis"), "Br J Anaesth 2023 Jun;130(6):719-728. doi: 10.1016/j.bja.2023.02.041. The outcome is pain on a scale of 0 to 10 and the covariate is the mean age of the participants."
      )
    ),
    conditionalPanel(
      condition = "output.metaoutcome=='Binary'",
      ns = ns,
      p("This default dataset for binary outcome data is from S. Dias, A.J. Sutton, N.J. Welton, and A.E. Ades (2013b),", tags$em("Heterogeneity - Subgroups, Meta-Regression, Bias, and Bias-Adjustment"), ", Medical Decision Making 33(5):618-640. The outcome is ACR-50, a reduction of at least 50% in the American College of Rhematology score, and the covariate is the mean disease duration of the participants."
      )
    ),
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
    output$metaoutcome <- reactive({ metaoutcome() })
    shiny::outputOptions(output, "metaoutcome", suspendWhenHidden = FALSE)
    
    output$download_long_data <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataLONG.csv", "data/Non_opioids_long.csv", "data/Certolizumab_long.csv")
  })
}