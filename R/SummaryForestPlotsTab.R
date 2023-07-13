
#' Create the UI for the summary forest plots tab.
#' This is a page with 2 columns, each containing a summary forest plot pane.
#' The left pane is for all of the studies and the right pane is for the filtered subset of studies.
#' 
#' 
#' @param id ID of the module
#' @return The created fluidPage
summary_forest_plots_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    summary_forest_plot_ui(id = ns('freqAll')),
    summary_forest_plot_ui(id = ns('freqSub'))
  )
}


#' Create the server for the summary forest plots tab.
#' 
#' @param id ID of the module
#' @param all_data Reactive value for accessing the frequentist analysis data for all studies
#' @param filtered_data Reactive value for accessing the frequentist analysis data for the filtered subset of studies
#' @param outcome_type Reactive value for accessing the type of outcome being measured
#' @return The created module server
summary_forest_plots_server <- function(id, all_data, filtered_data, outcome_type) {
  moduleServer(id, function(input, output, session) {
    
    # Axis type: any ratio outcomes are plotted on a log scale
    axis_type <- reactive({
      measure <- outcome_type()
      if (measure == "OR" || measure == "RR") {
        # Risk Ratio
        return("log")
      } else {
        # Every other outcome
        return("linear")
      }
    })
    
    summary_forest_plot_server(id = 'freqAll',
                               sfp_data = all_data,
                               axis_type = axis_type,
                               plot_title = 'Summary Forest Plot',
                               download_file_name = 'summary_forest_plot')
    summary_forest_plot_server(id = 'freqSub',
                               sfp_data = filtered_data,
                               axis_type = axis_type,
                               plot_title = 'Summary Forest Plot with Studies Excluded',
                               download_file_name = 'summary_forest_plot_filtered')
  })
}
