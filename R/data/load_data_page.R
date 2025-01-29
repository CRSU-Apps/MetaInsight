
#' Module UI for the data upload page.
#' 
#' @param id ID of the module
#' @return Div for the data upload page
load_data_page_ui <- function(id) {
  ns <- NS(id)
  div(
    sidebarLayout(
      sidebarPanel(
        data_input_panel_ui(id = ns('data_input_panel'))
      ),
      mainPanel(
        tabsetPanel(
          id = "instructions",
          long_format_upload_panel_ui(id = ns('long_upload')),
          wide_format_upload_panel_ui(id = ns('wide_upload')),
          tabPanel(
            title = "View Data",
            div(
              style = 'overflow-x: scroll',
              tableOutput(outputId = ns("tb"))
            )
          )
        )
      )
    )
  )
}


#' Module server for the data upload page.
#' 
#' @param id ID of the module
#' @return List of reactives:
#'   - 'data' is the uploaded data, wrangled such that the treatments are specified by IDs instead of names
#'   - 'is_default_data' is TRUE if data is an example data set, else FALSE if data has been uploaded
#'   - 'treatment_df' is the data frame containing the treatment ID ('Number') and the treatment name ('Label')
#'   - 'metaoutcome' is the outcome type selected
load_data_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_reactives <- data_input_panel_server(id = 'data_input_panel')
    data <- data_reactives$data
    is_default_data <- data_reactives$is_default_data
    treatment_list <- data_reactives$treatment_list
    metaoutcome <- data_reactives$metaoutcome
    
    ### Data analysis tab
    # Create a table which displays the raw data just uploaded by the user
    output$tb <- renderTable({
      if (is.null(data())) {
        return()
      }
      return(data())
    })
    
    long_format_upload_panel_server(id = 'long_upload', metaoutcome)
    wide_format_upload_panel_server(id = 'wide_upload', metaoutcome)
    
    wrangled_data <- reactive({
      return(WrangleUploadData(isolate(data()), treatment_list(), metaoutcome()))
    })
    
    wrangled_treatment_list <- reactive({
      CleanTreatmentIds(treatment_list())
    })
    
    return(
      list(
        data = wrangled_data,
        is_default_data = is_default_data,
        treatment_df = wrangled_treatment_list,
        metaoutcome = metaoutcome
      )
    )
  })
}