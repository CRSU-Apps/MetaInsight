
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
                    tabPanel(title = "View Data",
                             uiOutput(outputId = ns("tb")))
        ))))
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
replace_treatment_ids <- function(data, treatent_ids) {
  if ('T' %in% colnames(data)) {
    # Long format
    data$T <- treatent_ids$Number[match(data$T, treatent_ids$Label)]
  } else {
    # Wide format
    for (col in paste0('T.', seq(6))) {
      if (col %in% colnames(data)) {
        data[[col]] <- treatent_ids$Number[match(data[[col]], treatent_ids$Label)]
      } else {
        break
      }
    }
  }
  return(data)
}


#' Module server for the data upload tab.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
#' @return List of reactives:
#'   - 'data' is the uploaded data, wrangled such that the treatments are specified by IDs instead of names
#'   - 'treatment_df' is the data frame containing the treatment ID ('Number') and the treatment name ('Label')
load_data_page_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
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
      return(replace_treatment_ids(isolate(data()), treatment_list()))
    })
    
    return(list(data = wrangled_data,
                treatment_df = treatment_list))
  })
}