
#' Module UI for the bayesian forest plot and associated statistics.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_forest_plot_plus_stats_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(outputId = ns("bayesian_forest_plot")),
    fixedRow(
      p("Options to change limits of the x-axis:"),
      column(
        width = 6,
        align = 'center',
        numericInput(inputId = ns('axis_min'), label = "Minimum", value = 0.1, step = 0.1)
      ),
      column(
        width = 6,
        align = 'center',
        numericInput(inputId = ns('axis_max'), label = "Maximum", value = 5)
      )
    ),
    p("Model fit:"),
    tableOutput(outputId = ns("dic")),
    textOutput(outputId = ns("tau_text")),
    br(),
    br(),
    radioButtons(
      inputId = ns('download_format'),
      label = 'Document format',
      choices = c('PDF', 'PNG'),
      inline = TRUE
    ),
    downloadButton(outputId = ns('download_plot'))
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
        updateNumericInput(inputId = "axis_min", value=0.1)
        updateNumericInput(inputId = "axis_max", value=5)
      } else if (x == 'Continuous') {
        updateNumericInput(inputId = "axis_min", value=-10)
        updateNumericInput(inputId = "axis_max", value=10)
      }
    })

    # Forest plot

    # Forest plot for all studies
    output$forest_plot <- renderPlot({
      CreateForestPlot(model_output(), metaoutcome(), input$axis_min, input$axis_max)
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
    output$tau_text <-renderText({
      CreateTauSentence(model_output(), outcome_measure())
    })

    # Interactive UI
    output$bayesian_forest_plot <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
        outputId = ns("forest_plot"),
        width="630px",
        height = BayesPixels(
          as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
          title = TRUE
        )
      ), type = 6)
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(ifelse(analysis_type == "Full", 'All_studies.', 
                      ifelse(analysis_type == "Sub", 'Excluded_studies.',
                             'Regression_analysis.')),
               input$download_format)
      },
      content = function(file) {
        if (input$download_format == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else if (input$download_format == "PNG") {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        if (metaoutcome() == "Binary") {
          gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(log(input$axis_min), log(input$axis_max)))
        } else if (metaoutcome() == "Continuous") {
          gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(input$axis_min, input$axis_max))
        }
        if (analysis_type == "Regression") {
          mtext(model_output()$cov_value_sentence, side = 1, adj = 0)
        }
        dev.off()
      }
    )
    
  })
}
