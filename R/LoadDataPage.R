
#' Module UI for the data upload tab.
#' 
#' @param id ID of the module
#' @return Tab pane for the data upload page
load_data_page_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Load Data",
    htmlOutput(outputId = ns("CONBI")),
    tags$head(tags$style(paste0("#", ns("CONBI"), "{",
                                "color: white;
                                font-size: 20px;
                                font-style: bold;
                                background-color: #2196c4
                                }"))),
    br(),
    sidebarLayout(
      sidebarPanel(
        data_input_panel_ui(id = ns('data_input_panel'))
      ),
      mainPanel(
        tabsetPanel(id = "instructions",
                    long_format_upload_panel_ui(id = ns('long_upload')),
                    wide_format_upload_panel_ui(id = ns('wide_upload')),
                    tabPanel("View Data", 
                             p("Please double check if the total number of treatments matches the total number of treatment labels, 
                                i.e. make sure each treatment code in the data has a corresponding treatment label, 
                                and there is no additional treatment label which does not exist in the data."),
                             uiOutput(outputId = ns("tb")))
        ))))
}


#' Module server for the data upload tab.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
#' @return List of reactives:
#'   - 'data' is the uploaded data, wrangled such that the treatments are specified by IDs instead of names
#'   - 'treatment_list' is the string representation of the data frame containing the treatment ID ('Number')
#'     and the treatment name ('Label'), separated by tabs
load_data_page_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    ### Outcome selection
    output$CONBI <- renderText({
      paste("You have selected", "<font color=\"#ffd966\"><b>", metaoutcome(),"</b></font>", 
            "outcome on the 'Home' page. The instructions for formatting",
            "<font color=\"#ffd966\"><b>", metaoutcome(), "</b></font>", "outcomes are now displayed.")
    })
    
    data_reactives <- data_input_panel_server(id = 'data_input_panel', metaoutcome = metaoutcome)
    data <- data_reactives$data
    treatment_list <- data_reactives$treatment_list
    
    ### Data analysis tab
    # Create a table which displays the raw data just uploaded by the user
    output$tb <- renderTable({
      if (is.null(data())) {
        return()
      }
      return(data())
    })
    
    long_format_upload_panel_server(id = 'long_upload')
    wide_format_upload_panel_server(id = 'wide_upload')
    
    # Replace all of the treatment names with an ID
    wrangled_data <- reactive({
      df <- isolate(data())
      treatent_ids <- treatment_list()
      df$T <- treatent_ids$Number[match(df$T, treatent_ids$Label)]
      return(df)
    })
    
    formatted_treatment_list <- reactive({
      rows <- apply(treatment_list(), 1, function(row) {paste0(row[1], "\t", row[2])})
      return(paste(c("Number\tLabel", rows), collapse="\n"))
    })
    
    return(list(data = wrangled_data, treatment_list = formatted_treatment_list))
  })
}