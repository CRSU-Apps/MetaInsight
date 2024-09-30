
#' Module UI for the frquentist analysis panel.
#' 
#' @param id ID of the module
#' @return Div for the panel
frequentist_analysis_panel_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  ui = div(
    tabsetPanel(
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Forest Plot"),
        column(
          width = 6,
          uiOutput(outputId = ns("FreqForestPlot")),
          fixedRow(
            p("Options to change limits of the x-axis:"),
            column(
              width = 6,
              align = 'center',
              numericInput(inputId = ns('freqmin'), label = "Minimum", value = 0, step = 0.1)
            ),
            column(
              width = 6,
              align = 'center',
              numericInput(inputId = ns('freqmax'), label = "Maximum", value = 5, step = 1)
            )
          ),
          textOutput(outputId = ns("textcomp")),
          textOutput(outputId = ns("ref4")),
          radioButtons(
            inputId = ns('format_freq3'),
            label = 'Document format',
            choices = c('PDF', 'PNG'),
            inline = TRUE
          ),
          downloadButton(outputId = ns('downloadComp2'))
        ),
        column(
          width = 6,
          uiOutput(outputId = ns("FreqForestPlot_sub")),
          fixedRow(
            p("Options to change limits of the x-axis:"),
            column(
              width = 6,
              align = 'center',
              numericInput(inputId = ns('freqmin_sub'), label = "Minimum", value = 0, step = 0.1)
            ),
            column(
              width = 6,
              align = 'center',
              numericInput(inputId = ns('freqmax_sub'), label = "Maximum", value = 5, step = 1)
            )
          ),

          tags$style(
            glue::glue(
              "#{ns(\"ref_change\")} {{
               background-color: #ffd966;
                display:block;
              }}"
            )
          ),
          textOutput(outputId = ns("ref_change")),
          br(),
          textOutput(outputId = ns("text5")),
          textOutput(outputId = ns("ref3")),
          radioButtons(
            inputId = ns('format_freq4'),
            label = 'Document format',
            choices = c('PDF', 'PNG'),
            inline = TRUE
          ),
          downloadButton(outputId = ns('downloadComp'))
        )
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Comparison of all treatment pairs"),
        helpText("Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
        helpText("Relative treatment effects in ranked order for all studies"),
        tableOutput(outputId = ns("rankChartStatic")),
        downloadButton(outputId = ns('downloadRank'), label = "Download"),
        helpText("Relative treatment effects in ranked order with selected studies excluded"),
        tableOutput(outputId = ns("rankChartUpdating")),
        downloadButton(outputId = ns('downloadRankUpdate'), label = "Download")
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Inconsistency"),
        helpText("Assessment of inconsistency for all studies"),
        tableOutput(outputId = ns("Incon1")),
        downloadButton(outputId = ns('downloadIncon'), label = "Download"),
        helpText("Assessment of inconsistency with selected studies excluded"),
        tableOutput(outputId = ns("Incon2")),
        downloadButton(outputId = ns('downloadIncon2'), label = "Download")
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Summary forest plot"),
        summary_forest_plots_ui(id = ns('summary_forest_plot')))
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}


#' Module server for the frequentist analysis panel.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param bugsnetdt_sub Reactive containing bugsnet meta-analysis for sensitivity analysis
#' @param treatment_df Reactive data frame containing treatment IDs (Number) and names (Label)
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
frequentist_analysis_panel_server <- function(
    id,
    metaoutcome,
    outcome_measure,
    model_effects,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt,
    bugsnetdt_sub,
    treatment_df,
    reference_alter
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 2a. Forest Plot

    make_refText = function(ref) {
      y <- paste("All outcomes are versus the reference treatment:", ref)
      return(y)
    }
    
    # forest min and max values different if continuous/binary
    observe({
      if (outcome_measure() %in% c('OR', 'RR')) {
        updateNumericInput(inputId = "freqmin", value = 0.1, step = 0.1)
        updateNumericInput(inputId = "freqmin_sub", value = 0.1, step = 0.1)
        updateNumericInput(inputId = "freqmax", value = 5)
        updateNumericInput(inputId = "freqmax_sub", value = 5)
      } else if (outcome_measure() %in% c('MD', 'SMD', 'RD')) {
        updateNumericInput(inputId = "freqmin", value = -10, step = 1)
        updateNumericInput(inputId = "freqmin_sub", value = -10, step = 1)
        updateNumericInput(inputId = "freqmax", value = 10)
        updateNumericInput(inputId = "freqmax_sub", value = 10)
      } else {
        paste0("outcome_measure needs to be 'OR', 'RR', 'RD', 'MD', or 'SMD'")
      }
    })

    # Forest plot for all studies
    output$Comparison2<- renderPlot({
      make_netComp(freq = freq_all(), modelranfix = model_effects(), ref = reference_alter()$ref_all,
                   min = input$freqmin, max = input$freqmax
                   )
      title("Results for all studies")
    })

    # Text output displayed under forest plot
    output$textcomp<- renderText({
      texttau(freq_all(), outcome_measure(), model_effects())
    })

    output$ref4 <- renderText({
      make_refText(reference_alter()$ref_all)
    })


    # Forest plot with studies excluded
    output$SFPUpdatingComp <- renderPlot({
      make_netComp(freq = freq_sub(), modelranfix = model_effects(), ref = reference_alter()$ref_sub,
                   min = input$freqmin_sub, max = input$freqmax_sub)
      title("Results with selected studies excluded")
    })

    # Text output displayed under forest plot
    output$text5<- renderText({
      texttau(freq_sub(), outcome_measure(), model_effects())
    })

    output$ref3 <- renderText({
      make_refText(reference_alter()$ref_sub)
    })

    ### Interactive UI ###

    output$FreqForestPlot <- renderUI({
      plotOutput(
        outputId = ns("Comparison2"),
        height = BayesPixels(
          as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
          title = TRUE
        ),
        width = "630px"
      )
    })

    output$FreqForestPlot_sub <- renderUI({
      plotOutput(
        outputId = ns("SFPUpdatingComp"),
        height = BayesPixels(
          as.numeric(bugsnet_sumtb(bugsnetdt_sub(), metaoutcome())$Value[1]),
          title = TRUE
        ),
        width = "630px"
      )
    })

    output$downloadComp2 <- downloadHandler(
      filename = function() {
        paste0('All_studies.', input$format_freq3)
      },
      content = function(file) {
        if (input$format_freq3 == "PDF"){
          pdf(file= file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        make_netComp(freq_all(), model_effects(), reference_alter()$ref_all, input$freqmin, input$freqmax)
        dev.off()
      },
      contentType = "image/pdf"
    )

    output$downloadComp<- downloadHandler(
      filename = function() {
        paste0('Excluded_studies.', input$format_freq4)
      },
      content = function(file) {
        if (input$format_freq4 == "PDF") {
          pdf(file = file, width = 9, height=BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt_sub(), metaoutcome())$Value[1])))
        } else {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt_sub(), metaoutcome())$Value[1])))
        }
        make_netComp(freq_sub(), model_effects(), reference_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
        dev.off()
      }
    )
    
    output$ref_change <- renderText({
      if (identical(reference_alter()$ref_sub, reference_alter()$ref_all)=="FALSE") {
        paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", reference_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
      }
    })

    ### 2b. Comparison and rank table

    output$rankChartStatic <- renderTable(colnames=FALSE, {
      make_netrank(freq = freq_all(), modelranfix = model_effects(), rankopts = rank_option())
    })
    output$rankChartUpdating <- renderTable(colnames=FALSE, {
      make_netrank(freq = freq_sub(), modelranfix = model_effects(), rankopts = rank_option())
    })

    output$downloadRank <- downloadHandler(
      filename = 'Rank.csv',
      content = function(file) {
        write.csv(make_netrank(freq_all(), model_effects(), rank_option()), file)
      }
    )

    output$downloadRankUpdate <- downloadHandler(
      filename = 'Rank_sub.csv',
      content = function(file) {
        write.csv(make_netrank(freq_sub(), model_effects(), rank_option()), file)
      }
    )

    ### 2c. Inconsistency

    output$Incon1 <- renderTable(colnames=TRUE, make_Incon(freq_all(), model_effects()))
    output$Incon2 <- renderTable(colnames=TRUE, make_Incon(freq_sub(), model_effects()))

    output$downloadIncon <- downloadHandler(
      filename = 'Inconsistency.csv',
      content = function(file) {
        write.csv(make_Incon(freq_all(), model_effects()), file)
      }
    )

    output$downloadIncon2 <- downloadHandler(
      filename = 'Inconsistency_sub.csv',
      content = function(file) {
        write.csv(make_Incon(freq_sub(), model_effects()), file)
      }
    )
    
    ### 2d. Summary Forest Plot
    
    summary_forest_plots_server(id = 'summary_forest_plot',
                                all_data = freq_all,
                                treatment_df = treatment_df,
                                filtered_data = freq_sub,
                                outcome_type = outcome_measure,
                                desirability = rank_option,
                                model = isolate({
                                  model_effects
                                })
    )
    
  })
}