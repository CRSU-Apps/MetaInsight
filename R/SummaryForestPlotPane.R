
#' Create the UI for the summary forest plot pane.
#' This is a column containing a plot, radio buttons to select the download fie type,
#' and a download button.
#' 
#' @param id ID of the module
#' @return The created column
summary_forest_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    plotOutput(outputId = ns('summaryForestPlot')),
    radioButtons(inputId = ns('summaryForestPlotFormat'),
                 label = 'Document format',
                 choices = c('PDF', 'PNG'),
                 inline = TRUE),
    downloadButton(outputId = ns('downloadFreqSummaryForestPlot'))
  )
}


#' Create the server for the summary forest plot pane.
#' 
#' @param id ID of the module
#' @param sfp_data Reactive value for accessing the frequentist analysis data for the set of studies to plot
#' @param axis_type Reactive value for accessing the type of axis to plot
#' @param plot_title The title of the plot
#' @param download_file_name The name of the file to download the plot
#' @return The created module server
summary_forest_plot_server <- function(id, sfp_data, treatment_df, outcome_type, desirability, model, plot_title, download_file_name) {
  moduleServer(id, function(input, output, session) {
    # Setup plot
    output$summaryForestPlot <- renderPlot({
      CreateSummaryForestPlot(data_to_plot = sfp_data(),
                              treatment_df = treatment_df(),
                              plot_title = plot_title,
                              outcome_type = outcome_type(),
                              desirability = desirability(),
                              model = model())
    })
    
    # # Setup download button
    # output$downloadFreqSummaryForestPlot <- downloadHandler(
    #   filename = function() {
    #     paste0(download_file_name, '.', tolower(input$summaryForestPlotFormat))
    #   },
    #   content = function(file) {
    #     write_to_pdf_or_png(
    #       file,
    #       input$summaryForestPlotFormat,
    #       function() {
    #         CreateSummaryForestPlot(data_to_plot = sfp_data(),
    #                                    plot_title = plot_title,
    #                                    axis_type = axis_type())
    #       }
    #     )
    #   }
    # )
  })
}
