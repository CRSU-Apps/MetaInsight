
#' Module UI for the nodesplit model panel
#' 
#' @param id ID of the module
#' @return Div for the panel
nodesplit_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    p(
      "Please note: This may take more than 10 minutes depending on the number of treatment options. The node-splitting option for
      the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  We have produced a",
      tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Local-User-Guide", "guide",target="_blank"),
      "to running MetaInsight locally through RStudio on the user's own machine if they want to make use of this function."
    ),
    fluidRow(
      column(
        width = 6,
        p(tags$strong("Inconsistency test with node-splitting model for all studies")),
        actionButton(inputId = ns("node"), label = "Click here to run the node-splitting analysis for all studies"),
        tableOutput(outputId = ns("node_table")),
        downloadButton(outputId = ns('downloadnode'))
      ),
      column(
        width = 6,
        p(tags$strong("Inconsistency test with node-splitting model with selected studies excluded")),
        actionButton(inputId = ns("node_sub"), label = "Click here to run the node-splitting analysis with selected studies excluded"),
        tableOutput(outputId = ns("node_table_sub")),
        downloadButton(outputId = ns('downloadnode_sub'))
      )
    )
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
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
nodesplit_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    exclusions
    ) {
  moduleServer(id, function(input, output, session) {
    # Inconsistency test with notesplitting model for all studies
    model_nodesplit <- eventReactive(input$node, {
      nodesplit(data(), treatment_df(), metaoutcome(), outcome_measure(), model_effects())
    })

    output$node_table<- renderTable(colnames=TRUE, {
      model_nodesplit()
    })

    # Inconsistency test with notesplitting model with studies excluded
    model_nodesplit_sub <- eventReactive(input$node_sub, {
      nodesplit(data(), treatment_df(), metaoutcome(), outcome_measure(), model_effects(), exclusions())
    })

    output$node_table_sub<- renderTable(colnames=TRUE, {
      model_nodesplit_sub()
    })

    output$downloadnode <- downloadHandler(
      filename = 'Nodesplit.csv',
      content = function(file) {
        write.csv(model_nodesplit(), file)
      }
    )

    output$downloadnode_sub <- downloadHandler(
      filename = 'Nodesplit_sen.csv',
      content = function(file) {
        write.csv(model_nodesplit_sub(), file)
      }
    )
  })
}