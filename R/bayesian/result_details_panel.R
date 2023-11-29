
#' Module UI for the result details panel
#' 
#' @param id ID of the module
#' @return Div for the panel
result_details_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    fluidRow(
      column(
        width = 6,
        p(tags$strong("Results details for all studies")),
        verbatimTextOutput(outputId = ns("gemtc_results")),
        p(tags$strong("Gelman convergence assessment plot for all studies")),
        plotOutput(outputId = ns("gemtc_gelman"))
      ),
      column(
        width = 6,
        p(tags$strong("Results details with studies excluded")),
        verbatimTextOutput(outputId = ns("gemtc_results_sub")),
        p(tags$strong("Gelman convergence assessment plot with studies excluded")),
        plotOutput(outputId = ns("gemtc_gelman_sub"))
      )
    )
  )
}


#' Module server for the result details panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
result_details_panel_server <- function(
    id,
    model,
    model_sub
    ) {
  moduleServer(id, function(input, output, session) {

    # Results details for all studies
    output$gemtc_results <- renderPrint ({
      model()$sumresults
    })

    # Results details with studies excluded
    output$gemtc_results_sub <- renderPrint ({
      model_sub()$sumresults
    })

    # Gelman plots for all studies
    output$gemtc_gelman <- renderPlot ({
      gelman.plot(model()$mtcResults)
    })

    # Gelman plots with studies excluded
    output$gemtc_gelman_sub <- renderPlot ({
      gelman.plot(model_sub()$mtcResults)
    })
  })
}