
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
    p(tags$strong("Headings of columns are case sensitive.")),
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
      p(
        HTML(
          paste0(
            "This default dataset for continuous outcome data is from Gray, LJ. et al. A systematic review and mixed treatment 
            comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.
            The continuous outcome used is BMI loss (kg/m",tags$sup("2"),") 3 months from baseline."
          )
        )
      )
    ),
    conditionalPanel(
      condition = "output.metaoutcome=='Binary'",
      ns = ns,
      p(
        "This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
        Medical Decision Making, 18, 37-43.
        The binary outcome used is smoking cessation."
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
    
    output$download_long_data <- create_raw_data_download_handler(metaoutcome, "MetaInsightdataLONG.csv", "data/Cont_long.csv", "data/Binary_long.csv")
  })
}