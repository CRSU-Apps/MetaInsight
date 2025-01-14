
#' Module UI for the bayesian treatment comparisons page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_treatment_comparisons_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText("Please note: if you change the selections on the sidebar, you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
    p(
      tags$strong(
        "In contrast to the 'comparison of all treatment pairs' tab in the frequentist NMA results,
        this table only contains the estimates from the network meta analysis,
        i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence.
        If you would like to obtain the pairwise meta-analysis results, please run 3d. Nodesplit model"
      )
    ),
    br(),
    invalid_model_panel_ui(id = ns("model_invalid")),
    p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs.")),
    tableOutput(outputId = ns("baye_comparison")),
    downloadButton(outputId = ns('downloadbaye_comparison')),
    br(),
    br(),
    invalid_model_panel_ui(id = ns("model_invalid_sub")),
    p(tags$strong("Treatment effects with selected studies excluded: comparison of all treatment pairs.")),
    tableOutput(outputId = ns("baye_comparison_sub")),
    downloadButton(outputId = ns('downloadbaye_comparison_sub'))
  )
}


#' Module server for the bayesian treatment comparisons page.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_valid Reactive containing whether the model is valid
#' @param model_sub_valid Reactive containing whether the sensitivity analysis model is valid
bayesian_treatment_comparisons_page_server <- function(
    id,
    model,
    model_sub,
    outcome_measure,
    model_valid = reactiveVal(TRUE),
    model_sub_valid = reactiveVal(TRUE)
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    invalid_model_panel_server(id = "model_invalid_sub", model_valid = model_sub_valid)
    
    observe({
      if (!model_valid()) {
        shinyjs::disable(id="downloadbaye_comparison")
      } else {
        shinyjs::enable(id="downloadbaye_comparison")
      }
    })
    
    observe({
      if (!model_sub_valid()) {
        shinyjs::disable(id="downloadbaye_comparison_sub")
      } else {
        shinyjs::enable(id="downloadbaye_comparison_sub")
      }
    })

    # Treatment effects for all studies
    output$baye_comparison <- renderTable(
      rownames = TRUE,
      colnames = TRUE,
      {
        if (!model_valid()) {
          return()
        }
        baye_comp(model(), outcome_measure())
      }
    )

    # Treatment effects with studies excluded
    output$baye_comparison_sub <- renderTable (
      rownames = TRUE,
      colnames = TRUE,
      {
        if (!model_sub_valid()) {
          return()
        }
        baye_comp(model_sub(), outcome_measure())
      }
    )

    output$downloadbaye_comparison <- downloadHandler(
      filename = 'baye_comparison.csv',
      content = function(file) {
        write.csv(baye_comp(model(), outcome_measure()), file)
      }
    )

    output$downloadbaye_comparison_sub <- downloadHandler(
      filename = 'baye_comparison_sub.csv',
      content = function(file) {
        write.csv(baye_comp(model_sub(), outcome_measure()), file)
      }
    )
  })
}