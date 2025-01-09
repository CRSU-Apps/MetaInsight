
#' Module UI for the bayesian forest plot and associated statistics.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_forest_plot_plus_stats_ui <- function(id) {
  ns <- NS(id)
  div(
    shinycssloaders::withSpinner(
      uiOutput(outputId = ns("bayesian_forest_plot")),
      type = 6
    ),
    
    conditionalPanel(
      condition = "output.package == 'gemtc'",
      ns = ns,

      fixedRow(
        p("Options to change limits of the x-axis:"),
        column(
          width = 6,
          align = 'center',
          numericInput(inputId = ns('axis_min'), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = 'center',
          numericInput(inputId = ns('axis_max'), label = "Maximum", value = 5, step = 1)
        )
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
#' @param model_valid Reactive containing whether the model is valid
bayesian_forest_plot_plus_stats_server <- function(
    id,
    model_output,
    analysis_type,
    metaoutcome,
    outcome_measure,
    bugsnetdt,
    model_valid = reactiveVal(TRUE)
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (!model_valid()) {
        shinyjs::disable(id="download_plot")
      } else {
        shinyjs::enable(id="download_plot")
      }
    })

    output$package <- reactive({"gemtc"})
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)
    
    # forest min and max values different if continuous/binary
    observe({
      if (outcome_measure() %in% c("OR", "RR")) {
        updateNumericInput(inputId = "axis_min", value = 0.1, step = 0.1)
        updateNumericInput(inputId = "axis_max", value = 5)
      } else if (outcome_measure() %in% c("SMD", "MD", "RD")) {
        updateNumericInput(inputId = "axis_min", value = -10, step = 1)
        updateNumericInput(inputId = "axis_max", value = 10)
      } else {
        stop("outcome_measure needs to me 'OR', 'RR', 'RD', 'MD', or 'SMD'")
      }
    })
    
    # Forest plot

    # Forest plot for all studies
    output$forest_plot <- renderPlot({
      if (!model_valid()) {
        mtext("Please rerun model", side = 3, adj = 0, cex = 2)
        return()
      }
      
      CreateForestPlot(model_output(), metaoutcome(), input$axis_min, input$axis_max)
      title(paste(ifelse(analysis_type == "Full", "All studies:", 
                         ifelse(analysis_type == "Sub", "Results with studies excluded:",
                                "Regression analysis:")),
                  "
                  Bayesian",
                  model_output()$a, 
                  "consistency model forest plot results"))
      if (analysis_type == "Regression") {
        mtext(model_output()$cov_value_sentence, side = 1, adj = 0)
      }
    })
    
    # DIC table for all studies
    output$dic <- renderTable (
      digits = 3,
      rownames = TRUE,
      colnames = FALSE,
      {
        if (!model_valid()) {
          return()
        }
        model_output()$dic
      }
    )
    
    # Tau all studies
    output$tau_text <-renderText({
      if (!model_valid()) {
        return()
      }
      CreateTauSentence(model_output(), outcome_measure())
    })
    
    # Download
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

    # Interactive UI - forest plot
    output$bayesian_forest_plot <- renderUI({
      shinycssloaders::withSpinner(
        type = 6,
        plotOutput(
          outputId = ns("forest_plot"),
          width="630px",
          height = BayesPixels(
            as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
            title = TRUE
          )
        )
      )
    })
    
  })
}






#' Module server for the baseline risk forest plot and associated statistics.
#' 
#' @param id ID of the module
#' @param model_reactive The model output from bnma::network.run
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD" or "OR"
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param model_valid Reactive containing whether the model is valid
bayesian_forest_plot_plus_stats_baseline_risk_server <- function(
    id,
    model_reactive,
    metaoutcome,
    outcome_measure,
    bugsnetdt,
    model_valid = reactiveVal(TRUE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (!model_valid()) {
        shinyjs::disable(id="download_plot")
      } else {
        shinyjs::enable(id="download_plot")
      }
    })

    output$package <- reactive({"bnma"})
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)
    
    # Forest plot for all studies
    output$forest_plot <- renderPlot({
      if (!model_valid()) {
        mtext("Please rerun model", side = 3, adj = 0, cex = 2)
        return()
      }
      
      bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
    }) 
    
    # DIC table for all studies
    output$dic <- renderTable (
      digits = 3,
      rownames = TRUE,
      colnames = FALSE,
      {
        if (!model_valid()) {
          return()
        }
        BaselineRiskDicTable(model_reactive())
      }
    )
    
    # Tau all studies
    output$tau_text <-renderText({
      if (!model_valid()) {
        return()
      }
      CreateTauSentence(
        FormatForCreateTauSentence(model_reactive()),
        outcome_measure()
      )
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("Baseline_risk_forest.", input$download_format)
      },
      content = function(file) {
        if (input$download_format == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else if (input$download_format == "PNG") {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        
        if (metaoutcome() == "Binary") {
          bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
        } else if (metaoutcome() == "Continuous") {
          bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
        }
        dev.off()
      }
    )
    
    # Interactive UI - forest plot
    output$bayesian_forest_plot <- renderUI({
      shinycssloaders::withSpinner(
        type = 6,
        plotOutput(
          outputId = ns("forest_plot"),
          width="630px",
          height = BayesPixels(
            as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
            title = FALSE
          )
        )
      )
    })

  })
}
