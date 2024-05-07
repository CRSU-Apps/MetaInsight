
#' Module UI for the bayesian forest plots page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
bayesian_forest_plots_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      "Baysesian result using the gemtc package.",
      br(),
      "Heterogeneity prior: standard deviation ~ U(0,X), where X represents a ",
      tags$i("very large"),
      "difference in the analysis' outcome scale and is determined from the data.",
      br(),
      tags$i("Please note the outcome for continuous data has to be "),
      tags$b("mean difference"),
      tags$i(" for the Bayesian analysis. Standardised mean difference cannot be analysed."),
      br(),
      tags$i("Please note the outcome for binary data has to be "),
      tags$b("Odds Ratio or Risk Ratio"),
      tags$i(" for the Bayesian analysis. Risk difference cannot be analysed."),
      tags$strong("Please note each simulation may take 20 seconds.", style = "color:#FF0000")
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        p(tags$strong("Results for all studies")),
        p("Please click the button below to run Bayesian analysis for all studies, and after each time when you change the radiobutton selections."),
        actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
      ),
      column(
        width = 6,
        align = "center",
        p(tags$strong("Results with selected studies excluded")),
        p("Please click the button below to run each time after you finish the selection of studies, or change the radiobutton selections."),
        actionButton(inputId = ns("sub_do"), label = "Click here to run the sensitivity analysis")
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
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
      ),
      column(
        width = 6,
        align = "center",
        uiOutput(outputId = ns("BayesianForestPlot_sub")),
        fixedRow(
          p("Options to change limits of the x-axis:"),
          column(
            width = 6,
            align = 'center',
            numericInput(inputId = ns('bayesmin_sub'), label = "Minimum", value = 0.1)
          ),
          column(
            width = 6,
            align = 'center',
            numericInput(inputId = ns('bayesmax_sub'), label = "Maximum", value = 5)
          )
        ),
        tags$style(
          glue::glue(
            "#{ns(\"ref_change_bay\")} {{
              background-color: #ffd966;
              display:block;
            }}"
          )
        ),
        textOutput(outputId = ns("ref_change_bay")),
        p("Model fit:"),
        tableOutput(outputId = ns("dic_sub")),
        textOutput(outputId = ns("text_gemtc_sub")),
        br(),
        br(),
        radioButtons(
          inputId = ns('format4'),
          label = 'Document format',
          choices = c('PDF', 'PNG'),
          inline = TRUE
        ),
        downloadButton(outputId = ns('downloadBaye_plot_sub'))
      )
    )
  )
}


#' Module server for the bayesian forest plots page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param continuous_outcome Reactive containing acronym of the continuous outcome:
#'   "MD" for mean difference, or "SMD" for standardised mean difference
#' @param binary_outcome Reactive containing acronym of the binary outcome:
#'   "OR" for odds ratio, "RR" for risk ratio, or "RD" for risk difference
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
#'
#' @return List of reactives: "model" contains the full bayesian meta-analysis, "model_sub" contains the bayesian meta-analysis with studies excluded
bayesian_forest_plots_page_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    continuous_outcome,
    binary_outcome,
    model_effects,
    exclusions,
    bugsnetdt,
    reference_alter
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### SMD warning alert

    observeEvent(list(input$baye_do, input$sub_do), {
      if (continuous_outcome()=="SMD") {
        showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
      else if (binary_outcome()=="RD") {
        showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
    })

    # Bayesian analysis

    model <- eventReactive(input$baye_do, {
      bayesian_model(sub = FALSE, data(), treatment_df(), metaoutcome(), exclusions(),
                     outcome_measure(), model_effects(), reference_alter())
    })

    model_sub <- eventReactive(input$sub_do, {
      bayesian_model(sub = TRUE, data(), treatment_df(), metaoutcome(), exclusions(),
                     outcome_measure(), model_effects(), reference_alter())
    })
    
    # forest min and max values different if continuous/binary
    observe({
      x <- metaoutcome()
      if (x =='Binary') {
        updateNumericInput(inputId = "bayesmin", value=0.1)
        updateNumericInput(inputId = "bayesmin_sub", value=0.1)
        updateNumericInput(inputId = "bayesmax", value=5)
        updateNumericInput(inputId = "bayesmax_sub", value=5)
      } else {
        updateNumericInput(inputId = "bayesmin", value=-10)
        updateNumericInput(inputId = "bayesmin_sub", value=-10)
        updateNumericInput(inputId = "bayesmax", value=10)
        updateNumericInput(inputId = "bayesmax_sub", value=10)
      }
    })

    # Forest plot

    # Forest plot for all studies
    output$gemtc <- renderPlot({
      make_Forest(model(), metaoutcome(), input$bayesmin, input$bayesmax)
      title(paste("All studies:
              Bayesian", model()$a, "consistency model forest plot results"))
    })

    # DIC table for all studies
    output$dic <- renderTable ({
      model()$dic
    }, digits=3, rownames=TRUE, colnames=FALSE
    )

    # Tau all studies
    output$text_gemtc <-renderText({
      gemtctau(model(), outcome_measure())
    })

    # Forest plot with studies excluded
    output$gemtc_sub <- renderPlot({
      make_Forest(model_sub(), metaoutcome(), input$bayesmin_sub, input$bayesmax_sub)
      title(paste("Results with selected studies excluded:
              Bayesian", model_sub()$a,"consistency model forest plot results"))
    })

    # DIC table with studies excluded
    output$dic_sub <- renderTable ({
      model_sub()$dic
    }, digits=3, rownames=TRUE, colnames=FALSE)

    # Tau with studies excluded
    output$text_gemtc_sub <-renderText({
      gemtctau(model_sub(), outcome_measure())
    })

    # Interactive UI
    output$BayesianForestPlot <- renderUI({
      plotOutput(
        outputId = ns("gemtc"),
        width="630px",
        height = BayesPixels(
          as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
          title = TRUE
        )
      )
    })
    output$BayesianForestPlot_sub <- renderUI({
      plotOutput(
        outputId = ns("gemtc_sub"),
        width="630px",
        height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), metaoutcome())$Value[1]), title=TRUE)
      )
    })

    output$downloadBaye_plot <- downloadHandler(
      filename = function() {
        paste0('All_studies.', input$format2)
      },
      content = function(file) {
        if (input$format2 == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        if (metaoutcome() == "Binary") {
          gemtc::forest(model()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin), log(input$bayesmax)))
        }
        if (metaoutcome() == "Continuous") {
          gemtc::forest(model()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin, input$bayesmax))
        }
        dev.off()
      }
    )

    output$downloadBaye_plot_sub <- downloadHandler(
      filename = function() {
        paste0('Excluded_studies.', input$format4)
      },
      content = function(file) {
        if (input$format4 == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), metaoutcome())$Value[1])))
        } else {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), metaoutcome())$Value[1])))
        }
        if (metaoutcome() == "Binary") {
          gemtc::forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin_sub), log(input$bayesmax_sub)))
        }
        if (metaoutcome() == "Continuous") {
          gemtc::forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin_sub, input$bayesmax_sub))
        }
        dev.off()
      }
    )
    
    output$ref_change_bay <- renderText({
      if (identical(reference_alter()$ref_sub, reference_alter()$ref_all)=="FALSE") {
        paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", reference_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
      }
    })
    
    return(
      list(
        model = model,
        model_sub = model_sub
      )
    )
  })
}