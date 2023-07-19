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
    
    find_all <- function(data, field_name) {
      if (field_name %in% colnames(data)) {
        # Long format
        return(unique(data[[field_name]]))
      } else {
        # Wide format
        all <- c()
        for (i in seq(6)) {
          all <- c(all, data[[paste0(field_name, ".", i)]])
        }
        return(unique(all))
      }
    }
    
    treatment_list <- reactive({
      treatment_names = find_all(data(), "T")
      return(data.frame(Number = seq(1, length(treatment_names)), Label = treatment_names))
    })
    
    wrangled_data <- reactive({
      df <- isolate(data())
      treatent_ids <- treatment_list()
      df$T <- treatent_ids$Number[match(unlist(df$T), treatent_ids$Label)]
      return(df)
    })
    
    formatted_treatment_list <- reactive({
      rows <- apply(treatment_list(), 1, function(row) {paste0(row[1], "\t", row[2])})
      return(paste(c("Number\tLabel", rows), collapse="\n"))
    })
    
    return(list(data = wrangled_data, treatment_list = formatted_treatment_list))
  })
}