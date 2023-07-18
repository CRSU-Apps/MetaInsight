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
      data_input_panel_ui(id = ns('data_input_panel')),
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
    
    return(list(data = data, treatment_list = treatment_list))
  })
}