
#' Module UI for the bayesian forest plot and associated statistics.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_forest_plot_plus_stats_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(outputId = ns("BayesianForestPlot")),
    fixedRow(
      p("Options to change limits of the x-axis:"),
      column(
        width = 6,
        align = 'center',
        numericInput(inputId = ns('bayesmin'), label = "Minimum", value = 0.1)
      ),
      column(
        width = 6,
        align = 'center',
        numericInput(inputId = ns('bayesmax'), label = "Maximum", value = 5)
      )
    ),
    p("Model fit:"),
    tableOutput(outputId = ns("dic")),
    textOutput(outputId = ns("text_gemtc")),
    br(),
    br(),
    radioButtons(
      inputId = ns('format2'),
      label = 'Document format',
      choices = c('PDF', 'PNG'),
      inline = TRUE
    ),
    downloadButton(outputId = ns('downloadBaye_plot'))
  )
}


#' Module server for the bayesian forest plot and associated statistics.
#' 
#' @param id ID of the module
#' @param model_output Containing the full bayesian meta-analysis with associated outputs
#' @param analysis_type Whether the analysis is a base analysis ('Full'), sensitivity analysis ('Sub') or a regression analysis ('Regression')
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
bayesian_forest_plot_plus_stats_server <- function(
    id,
    model_output,
    analysis_type,
    metaoutcome,
    outcome_measure,
    bugsnetdt
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # forest min and max values different if continuous/binary
    observe({
      x <- metaoutcome()
      if (x == 'Binary') {
        updateNumericInput(inputId = "bayesmin", value=0.1)
        updateNumericInput(inputId = "bayesmax", value=5)
      } else if (x == 'Continuous') {
        updateNumericInput(inputId = "bayesmin", value=-10)
        updateNumericInput(inputId = "bayesmax", value=10)
      }
    })

    # Forest plot

    # Forest plot for all studies
    output$gemtc <- renderPlot({
      make_Forest(model_output(), metaoutcome(), input$bayesmin, input$bayesmax)
      title(paste(ifelse(analysis_type == "Full", "All studies:", 
                         ifelse(analysis_type == "Sub", "Results with studies excluded:",
                                "Regression analysis:")),
                  "
                  Bayesian",
                  model_output()$a, 
                  "consistency model forest plot results"))
    })

    # DIC table for all studies
    output$dic <- renderTable ({
      model_output()$dic
    }, digits=3, rownames=TRUE, colnames=FALSE
    )

    # Tau all studies
    output$text_gemtc <-renderText({
      gemtctau(model_output(), outcome_measure())
    })

    # Interactive UI
    output$BayesianForestPlot <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
        outputId = ns("gemtc"),
        width="630px",
        height = BayesPixels(
          as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
          title = TRUE
        )
      ), type = 6)
    })

    output$downloadBaye_plot <- downloadHandler(
      filename = function() {
        paste0(ifelse(analysis_type == "Full", 'All_studies.', 
                      ifelse(analysis_type == "Sub", 'Excluded_studies.',
                             'Regression_analysis.')),
               input$format2)
      },
      content = function(file) {
        if (input$format2 == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        if (metaoutcome() == "Binary") {
          gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin), log(input$bayesmax)))
        }
        if (metaoutcome() == "Continuous") {
          gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin, input$bayesmax))
        }
        if (analysis_type == "Regression") {
          mtext(model_output()$cov_value_sentence, side = 1, adj = 0)
        }
        dev.off()
      }
    )
    
  })
}
