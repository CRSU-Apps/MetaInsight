
#' Module UI for the wide format data upload instructions.
#' 
#' @param id ID of the module
#' @return Tab panel containing data upload instructions and example data downloads
wide_format_upload_panel_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Wide format upload",
    h2(tags$strong("Instructions for uploading wide format data")),
    br(),
    p(
      tags$strong(
        "MetaInsight allows data in either long format, or wide format. This tab provides instructions for wide format data, where each row contains all the treatment arms from one study. 
        Instructions are as below.
        Please note that MetaInsight is not compatible with studies containing multiple arms of the same treatment."
      )
    ),
    h4(tags$strong("Step 1:")),
    p("The wide format data file should contain the following columns:"),
    p(tags$ul(tags$li(tags$strong("Study"), "contains name (e.g., author,year) of the study. The study name must be unique for each study."))),
    p(tags$ul(tags$li(tags$strong("T.1, T.2, ..., up to T.k"), "contains name of the treatment given for study arm 1, 2, ..., up to k, respectively. Treatment names should only contain letters, numbers and underscores."))),
    conditionalPanel(
      condition = "output.metaoutcome == 'Continuous'",
      ns = ns,
      p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.k"), "contains number of participants in study arm 1, 2, ..., up to k, respectively"))),
      p(tags$ul(tags$li(tags$strong("Mean.1, Mean.2, ..., up to Mean.k"), "contains the mean value of the outcome in study arm 1, 2, ..., up to k, respectively"))),
      p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.k"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to k, respectively")))
    ),
    conditionalPanel(
      condition = "output.metaoutcome == 'Binary'",
      ns = ns,
      p(tags$ul(tags$li(tags$strong("R.1, R.2, ..., up to R.k"), "contains number of participants with the outcome of interest in study arm 1, 2, ..., up to k, respectively"))),
      p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.k"), "contains number of participants in study arm 1, 2, ..., up to k, respectively")))
    ),
    p("The wide format data file may also contain the following columns:"),
    tags$ul(
      tags$li(
        tags$strong("covar.<COVARIATE_NAME>"),
        " contains the study-level covariate value, where <COVARIATE_NAME> is replaced by the name of the covariate. The name of the covariate will be extracted and used in the analysis output."
      )
    ),
    tags$ul(
      tags$li(
        tags$strong("rob"),
        " contains the risk of bias of the study as required by", tags$a(href = "https://cinema.ispm.unibe.ch/", "CINeMA", target = "_blank"), "."
      )
    ),
    tags$ul(
      tags$li(
        tags$strong("indirectness"),
        " contains the indirectness of the study as required by", tags$a(href = "https://cinema.ispm.unibe.ch/", "CINeMA", target = "_blank"), "."
      )
    ),
    p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
    p("The csv file that is used to produce the example dataset can be downloaded from here:"),
    downloadButton(
      outputId = ns("downloadDataWide"),
      label = "Download the example dataset in wide format"
    ),
    br(),
    h4(tags$strong("Step 2:")),
    p('Select the reference treatment in the drop-down box. By default, MetaInsight will select a treatment which matches common names for a standard reference treatment.'),
    hr(),
    conditionalPanel(
      condition= "output.metaoutcome=='Continuous'",
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
    p(
      tags$strong(
        "Note: The default dataset, pre-loaded on the 'View Data' tab,
        will be used for analysis if no file is selected. The 'View Data'
        tab will automatically update once a file is successfully loaded."
      )
    )
  )
}

#' Module server for the wide format data upload instructions.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
wide_format_upload_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    output$metaoutcome <- reactive({ metaoutcome() })
    shiny::outputOptions(output, "metaoutcome", suspendWhenHidden = FALSE)
    
    output$downloadDataWide <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataWIDE.csv", "data/Non_opioids_wide.csv", "data/Certolizumab_wide.csv")
  })
}