#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      div(
        tabsetPanel(
          tabPanel(
            title = "4b-1. Regression plot",
            div(
              helpText(
                "Baysesian baseline risk meta-regression using the bnma package.",
                br(),
                "Heterogeneity prior: standard deviation ~ U(0,5) or U(0,100) for a binary or continuous outcome respectively.",
                br(),
                tags$strong("Please note each simulation may take 60 seconds.", style = "color:#FF0000")
              ),
              fixedRow(
                align = "center",
                p(tags$strong("Results for all studies")),
                p("Please choose your regressor type, then click the button below to run meta-regression analysis (and each time you subsequently change any options)."),
                fluidRow(
                  div(selectInput(inputId = ns("select_regressor"),
                                  label = "Choose type of regression coefficient",
                                  choices = c("shared", "exchangeable", "unrelated")),
                      style = "display: inline-block;"),
                  div(
                    shinyWidgets::dropMenu(shinyWidgets::dropdownButton(
                      size = 'xs', status = "info", icon=icon('info')), align = 'left',
                      p(tags$strong("Types of regressors")),
                      p(tags$u("Shared:"), " Coefficient is the same for all treatment comparisons"),
                      p(tags$u("Exchangeable:"), " Coefficient is different for each treatment comparison but all come from a shared distribution"),
                      p(tags$u("Unrelated:"), " Coefficient is different for each treatment comparison")),
                    style = "display:inline-block; vertical-align: top;"
                  )
                ),
                actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
              )
            ),
          ),
          tabPanel(
            title = "4b-2. Forest plot",
            fixedRow(
              align = "center",
              
              div(
                uiOutput(outputId = ns("bayesian_forest_plot")),
                uiOutput(outputId = ns("convergence_warning")),
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
              
            )
          )
        )
      )
    )
  )
}






#' Create the baseline risk analysis server.
#'
#' @param id ID of the module
baseline_risk_analysis_panel_server <- function(    id, 
                                                    all_data,
                                                    treatment_df,
                                                    reference_treatment,
                                                    metaoutcome,
                                                    outcome_measure,
                                                    model_effects,
                                                    bugsnetdt
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    model_reactive <- eventReactive(input$baye_do, {
      BaselineRiskRegression(br_data = all_data(),
                             treatment_ids = treatment_df(),
                             outcome_type = metaoutcome(),
                             ref = treatment_df()$Label[match(1, treatment_df()$Number)],
                             effects_type = model_effects(),
                             cov_parameters = input$select_regressor)
    })
    
    #Outcome in the reference arm, needed for the graph
    covariate_data <- reactive({GetReferenceOutcome(data = all_data(),
                                                    treatments = treatment_df()$Label[order(treatment_df()$Number)],
                                                    outcome_type = metaoutcome())
      })
    
    #Warn if the model did not converge
    output$convergence_warning <- renderText({
        ifelse(model_reactive()$max.gelman > 1.05,
               "<b><h3> Warning: Model did not converge according to Gelman-Rubin diagnostics </h3></b>",
               "")
    })
      
    # Forest plot for all studies
    output$forest_plot <- renderPlot({
      bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
    })
    
    observe({
      model_reactive()
      # DIC table for all studies
      output$dic <- renderTable ({
        BaselineRiskDicTable(model_reactive())
      }, digits=3, rownames=TRUE, colnames=FALSE)
      # Tau all studies
      output$tau_text <-renderText({
        CreateTauSentence(FormatForCreateTauSentence(model_reactive()), outcome_measure())
      })
    })
    
    # Interactive UI
    output$bayesian_forest_plot <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
          outputId = ns("forest_plot"),
          width="630px",
          height = BayesPixels(
            as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
            title = FALSE
          )
        ), type = 6)
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
    
    })
}

