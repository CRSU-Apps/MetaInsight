
#' Module UI for the covariate regression treatment comparisons page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
covariate_treatment_comparisons_page_ui <- function(id) {
  ns <- NS(id)
  div(
    conditionalPanel(
      condition = "output.package == 'gemtc'",
      ns = ns,
      tags$strong(
        p(
          "This table only contains the estimates from the network meta analysis,
          i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence.
          If you would like to obtain the pairwise meta-analysis results, please run 4c-4. Nodesplit model"
        )
      ),
      br()
    ),
    invalid_model_panel_ui(id = ns("model_invalid")),
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
#' @param model_valid Reactive containing whether the model is valid
covariate_treatment_comparisons_page_server <- function(
    id,
    model,
    outcome_measure,
    model_valid
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    
    observe({
      if (is.null(model_valid()) || !model_valid()) {
        shinyjs::disable(id="downloadbaye_comparison")
      } else {
        shinyjs::enable(id="downloadbaye_comparison")
      }
    })

    # Treatment effects for all studies
    output$baye_comparison <- renderTable(
      rownames = TRUE,
      colnames = TRUE,
      {
        if (is.null(model_valid()) || !model_valid()) {
          return()
        }
        baye_comp(model(), outcome_measure())
      }
    )
    
    output$package <- reactive({"gemtc"})
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)
    
    output$cov_value_statement <- renderText({
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
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
#' @param model_valid Reactive containing whether the model is valid
treatment_comparisons_page_baseline_risk_server <- function(
    id,
    model,
    outcome_measure,
    model_valid = reactiveVal(TRUE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    
    observe({
      if (is.null(model_valid()) || !model_valid()) {
        shinyjs::disable(id="downloadbaye_comparison")
      } else {
        shinyjs::enable(id="downloadbaye_comparison")
      }
    })
    
    # Treatment effects for all studies
    output$baye_comparison <- renderTable(
      {
        if (is.null(model_valid()) || !model_valid()) {
          return()
        }
        BaselineRiskRelativeEffectsTable(
          bnma::relative.effects.table(model(), summary_stat = "ci")
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