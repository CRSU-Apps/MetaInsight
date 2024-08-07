
#' Module UI for the covariate regression treatment comparisons page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
covariate_treatment_comparisons_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText("Please note: if you change the selections on the sidebar, you will need to re-run the analysis from the 'Forest Plot' page."),
    p(
      tags$strong(
        conditionalPanel(condition = "output.package == 'gemtc'",
                         ns = ns,
                         p("This table only contains the estimates from the network meta analysis,
        i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence.
        If you would like to obtain the pairwise meta-analysis results, please run 4c-4. Nodesplit model")
        )
      )
    ),
    br(),
    p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs.")),
    tableOutput(outputId = ns("baye_comparison")),
    conditionalPanel(condition = "output.package == 'gemtc'",
                     ns = ns,
                     textOutput(outputId = ns('cov_value_statement'))
                     ),
    downloadButton(outputId = ns('downloadbaye_comparison'))
  )
}


#' Module server for the covariate regression treatment comparisons page.
#' 
#' @param id ID of the module
#' @param model Reactive containing covariate regression meta-analysis for all studies
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
covariate_treatment_comparisons_page_server <- function(
    id,
    model,
    outcome_measure
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Treatment effects for all studies
    output$baye_comparison <- renderTable ({
      baye_comp(model(), outcome_measure())
    }, rownames=TRUE, colnames = TRUE
    )
    
    output$package <- reactive({"gemtc"})
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)
    
    output$cov_value_statement <- renderText({
      model()$cov_value_sentence
    })

    output$downloadbaye_comparison <- downloadHandler(
      filename = 'regression_comparison.csv',
      content = function(file) {
        write.csv(baye_comp(model(), outcome_measure()), file)
      }
    )
  })
}






#' Module server for the baseline risk regression treatment comparisons page.
#' 
#' @param id ID of the module
#' @param model Reactive containing covariate regression meta-analysis for all studies
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD" or "OR"
treatment_comparisons_page_baseline_risk_server <- function(
    id,
    model,
    outcome_measure
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Treatment effects for all studies
    output$baye_comparison <- renderTable(
      {
        BaselineRiskRelativeEffectsTable(
          bnma::relative.effects.table(model(),
                                       summary_stat = "ci")
        )
      },
      rownames = TRUE,
      colnames = TRUE
    )
    
    output$package <- reactive({"bnma"})
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)

    output$downloadbaye_comparison <- downloadHandler(
      filename = 'baseline_risk_comparison.csv',
      content = function(file) {
        write.csv(
          BaselineRiskRelativeEffectsTable(bnma::relative.effects.table(model(), summary_stat = "ci")), file
        )
      }
    )
  })
}