
#' Module UI for the nodesplit model panel
#' 
#' @param id ID of the module
#' @param item_name Name of this nodesplit item.
#' @return Div for the panel
nodesplit_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    actionButton(inputId = ns("node"), label = glue::glue("Click here to run the nodesplitting analysis for {item_name}")),
    uiOutput(outputId = ns("node_plot_placeholder")),
    radioButtons(
      inputId = ns('download_format'),
      label = 'Document format',
      choices = c('PDF', 'PNG'),
      inline = TRUE
    ),
    downloadButton(outputId = ns('downloadnode'))
     
  )
}


#' Module server for the nodesplit model panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
nodesplit_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    model_nodesplit <- eventReactive(
      input$node,
      {
        return(
          nodesplit(
            data(),
            treatment_df(),
            metaoutcome(),
            outcome_measure(),
            model_effects()
          )
        )
      }
    )
    
    ncomp <- reactive({as.numeric(length(model_nodesplit())-1)}) # number of comparisons
  
    
    output$node_plot_placeholder <- renderUI({
      plotOutput(
        outputId = ns("node_plot"),
        height = NodePixels(ncomp())
      )
    })
    
    output$node_plot<- renderPlot({
      req(input$node)
      plot(summary(model_nodesplit()), digits = 3)
    }
    )

    output$downloadnode <- downloadHandler(
      filename = function() {
        paste0("Nodesplit.",
               input$download_format)
      },
      content = function(file) {
        if (input$download_format == "PDF") {
          pdf(file = file, width = 9, height = NodeDownloadHeight(ncomp(), 'in'))
        } else if (input$download_format == "PNG") {
          png(file = file, width = 850, height = NodeDownloadHeight(ncomp(), 'px'))
        }
        plot(summary(model_nodesplit()), digits = 3)
        dev.off()
      },
      contentType = "image/pdf"
    )
  })
}