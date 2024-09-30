
#' Create the UI for the summary forest plot pane.
#' This is a column containing a plot, radio buttons to select the download fie type,
#' and a download button.
#' 
#' @param id ID of the module
#' @return The created column
summary_forest_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    plotOutput(outputId = ns('summaryForestPlot'), height = "700px"),
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
#' @param sfp_data Reactive value for the frequentist analysis data for the set of studies to plot
#' @param treatment_df Data frame containing the names and IDs of all treatments
#' @param outcome_type Reactive value for the type of outcome being plotted
#' @param desirability Reactive value for whether small values are "good" or "bad"
#' @param model Reactive value for whether the model is "random" or "fixed" effects
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
                              model = model()
      )
    })
    
    n_treatments <- reactive(length(treatment_df()$Label))
    
    # Setup download button
    output$downloadFreqSummaryForestPlot <- downloadHandler(
      filename = function() {
        paste0(download_file_name, '.', tolower(input$summaryForestPlotFormat))
      },
      content = function(file) {
        write_to_pdf_or_png(
          file = file,
          type = input$summaryForestPlotFormat,
          renderFunction = function() {
            CreateSummaryForestPlot(data_to_plot = sfp_data(),
                                    treatment_df = treatment_df(),
                                    plot_title = plot_title,
                                    outcome_type = outcome_type(),
                                    desirability = desirability(),
                                    model = model())
          },
          height = 2.5 * n_treatments(),
          width = 2.5 * n_treatments(),
          png_units = "in"
        )
      }
    )
  })
}
