
#' Module UI for the nodesplit model panel
#' 
#' @param id ID of the module
#' @return Div for the panel
nodesplit_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    actionButton(inputId = ns("node"), label = "Click here to run the nodesplitting analysis for all studies"),
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
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis. Optional.
nodesplit_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    exclusions = NA
    ) {
  moduleServer(id, function(input, output, session) {
    
    model_nodesplit <- eventReactive(
      input$node,
      {
        excl <- NA
        if (!is.na(exclusions)) {
          excl <- exclusions()
        }
        return(
          nodesplit(
            data(),
            treatment_df(),
            metaoutcome(),
            outcome_measure(),
            model_effects(),
            excl
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