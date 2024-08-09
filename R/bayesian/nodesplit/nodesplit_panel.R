
#' Module UI for the nodesplit model panel
#' 
#' @param id ID of the module
#' @param item_name Name of this nodesplit item.
#' @return Div for the panel
nodesplit_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    actionButton(inputId = ns("node"), label = glue::glue("Click here to run the nodesplitting analysis for {item_name}")),
    tableOutput(outputId = ns("node_table")),
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

    output$node_table<- renderTable(
      colnames = TRUE,
      {
        model_nodesplit()
      }
    )

    output$downloadnode <- downloadHandler(
      filename = 'Nodesplit.csv',
      content = function(file) {
        write.csv(model_nodesplit(), file)
      }
    )
  })
}