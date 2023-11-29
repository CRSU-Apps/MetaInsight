
#' Module UI for the bayesian analysis panel
#' 
#' @param id ID of the module
#' @return Div for the panel
bayesian_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    tabsetPanel(
      tabPanel(
        title = "3a. Forest plot",
        bayesian_forest_plots_page_ui(id = ns("forest_plots"))
      ),
      tabPanel(
        title = "3b. Comparison of all treatment pairs",
        bayesian_treatment_comparisons_page_ui(id = ns("treatment_comparisons"))
      ),
      tabPanel(
        title = "3c. Ranking Panel",
        ranking_panel_ui(id = ns("ranking"))
      ),
      tabPanel(
        title = "3d. Nodesplit model",
        helpText(
          "Please note: if you change the selections on the sidebar,
          you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
        ),
        p(
          "Please note: This may take more than 10 minutes depending on the number of treatment options. The node splitting option for
          the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  We have produced a",
          tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Local-User-Guide", "guide",target="_blank"),
          "to running MetaInsight locally through RStudio on the user's own machine if they want to make use of this function."
        ),
        fluidRow(
          column(
            width = 6,
            p(tags$strong("Inconsistency test with notesplitting model for all studies")),
            actionButton(inputId = ns("node"), label = "Click here to run the nodesplitting analysis for all studies"),
            tableOutput(outputId = ns("node_table")),
            downloadButton(outputId = ns('downloadnode'))
          ),
          column(
            width = 6,
            p(tags$strong("Inconsistency test with notesplitting model with studies excluded")),
            actionButton(inputId = ns("node_sub"), label = "Click here to run the nodesplitting analysis with studies excluded"),
            tableOutput(outputId = ns("node_table_sub")),
            downloadButton(outputId = ns('downloadnode_sub'))
          )
        )
      ),
      tabPanel(
        title = "3e. Bayesian result details",
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
      ),
      tabPanel(
        title = "3f. Deviance report",
        helpText(
          "Please note: if you change the selections on the sidebar,
          you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
        ),
        p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
        fluidRow(
          column(
            width = 6,
            p(tags$strong("Residual deviance from NMA model and UME inconsistency model for all studies")),
            plotlyOutput(outputId = ns("dev_scat"))
          ),
          column(
            width = 6,
            p(tags$strong("Residual deviance from NMA model and UME inconsistency model with studies excluded")),
            plotlyOutput(outputId = ns("dev_scat_sub"))
          )
        ),
        p(
          "This plot represents each data points' contribution to the residual deviance for the
          NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models
          (vertical axis) along with the line of equality. The points on the equality line means there is no
          improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency.
          Points above the equality line means they have a smaller residual deviance for the consistency model indicating a
          better fit in the NMA consistency model and points below the equality line
          means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model
          may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
          decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
        ),
        br(),
        br(),
        br(),
        fluidRow(
          column(
            width = 6,
            p(tags$strong("Per-arm residual deviance for all studies")),
            plotlyOutput(outputId = ns("dev1"))
          ),
          column(
            width = 6,
            p(tags$strong("Per-arm residual deviance for sensitivity analysis")),
            plotlyOutput(outputId = ns("dev1_sub"))
          ),
          br(),
          p(
            "This stem plot represents the posterior residual deviance per study arm. The total number of stems equals
            the total number of data points in the network meta analysis. Going from left to right, the alternating symbols
            on the stems indicate the different studies. Each stem corresponds to the residual deviance ($dev.ab) associated with each
            arm in each study. The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each
            data point. You can identify which stem corresponds to which study arm by hovering on the stem symbols.
            (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
            decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
          ),
          br(),
          br(),
          br(),
          column(
            width = 6,
            p(tags$strong("Leverage plot for all studies")),
            plotlyOutput(outputId = ns("dev2"))
          ),
          column(
            width = 6,
            p(tags$strong("Leverage plot for sensitivity analysis")),
            plotlyOutput(outputId = ns("dev2_sub"))
          )
        ),
        br(),
        p(
          "This leverage plot shows the average leverage across the arms for each study ({sum($lev.ab)}/{number of arms}
          for each study) versus the square root of the average residual deviance across the arms for each study
          (sqrt({sum($dev.ab)}/{number of arms}) for each study).
          The leverage for each data point, is calculated as the posterior mean of the residual
          deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to
          identify influential and/or poorly fitting studies and can be used to check how each study is affecting
          the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root
          of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas
          each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with
          c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are
          influential, which means that they have a strong influence on the model parameters that generate their fitted
          values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
          decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.
          Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4,
          pp.583-639)"
        ),
        br(),
        br()
      ),
      tabPanel(
        title = "3g. Model details",
        helpText(
          "Please note: if you change the selections on the sidebar,
          you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
        ),
        tabsetPanel(
          tabPanel(
            title = "3g-1. Model codes",
            p(tags$strong("Model codes for analysis of all studies")),
            downloadButton(outputId = ns('download_code')),
            verbatimTextOutput(outputId = ns("code"))
          ),
          tabPanel(
            title = "3g-2. Initial values",
            p(tags$strong("Initial values")),
            downloadButton(outputId = ns('download_inits_1'), "Download initial values for chain 1"),
            downloadButton(outputId = ns('download_inits_2'), "Download initial values for chain 2"),
            downloadButton(outputId = ns('download_inits_3'), "Download initial values for chain 3"),
            downloadButton(outputId = ns('download_inits_4'), "Download initial values for chain 4"),
            verbatimTextOutput(outputId = ns("inits"))
          ),
          tabPanel(
            title = "3g-3. Download simulations",
            p(tags$strong("Download simulated data")),
            downloadButton(outputId = ns('download_data1'), "Download data from chain 1"),
            br(),
            downloadButton(outputId = ns('download_data2'), "Download data from chain 2"),
            br(),
            downloadButton(outputId = ns('download_data3'), "Download data from chain 3"),
            br(),
            downloadButton(outputId = ns('download_data4'), "Download data from chain 4")
          ),
          tabPanel(
            title = "3g-4. Deviance details",
            fluidRow(
              column(
                width = 6,
                p(tags$strong("Deviance data for all studies")),
                p("NMA (consistency) model"),
                verbatimTextOutput(outputId = ns("dev"))
              ),
              column(
                width = 6,
                p(tags$strong("Deviance data for sensitivity analysis")),
                p("NMA (consistency) model"),
                verbatimTextOutput(outputId = ns("dev_sub"))
              )
            ),
            fluidRow(
              column(
                width = 6,
                p("UME (inconsistency) model"),
                verbatimTextOutput(outputId = ns("dev_ume"))
              ),
              column(
                width = 6,
                p("UME (inconsistency) model"),
                verbatimTextOutput(outputId = ns("dev_ume_sub"))
              )
            )
          )
        )
      )
    )
  )
}


#' Module server for the bayesian analysis panel.
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
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param reference_alter Reactive containing the name of the reference treatment for the sensitivity
#'  analysis accounting for if the chosen reference treatment has been excluded
bayesian_analysis_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    continuous_outcome,
    binary_outcome,
    model_effects,
    exclusions,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt,
    reference_alter
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### SMD warning alert

    observeEvent(list(input$node, input$node_sub), {
      if (continuous_outcome()=="SMD") {
        showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
      else if (binary_outcome()=="RD") {
        showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
      }
    })
    
    # 3a. Forest plots
    forest_plots_reactives <- bayesian_forest_plots_page_server(
      id = "forest_plots",
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      continuous_outcome = continuous_outcome,
      binary_outcome = binary_outcome,
      model_effects = model_effects,
      exclusions = exclusions,
      bugsnetdt = bugsnetdt,
      reference_alter = reference_alter
    )
    
    model <- forest_plots_reactives$model
    model_sub <- forest_plots_reactives$model_sub


    # 3b. Comparison of all treatment pairs
    bayesian_treatment_comparisons_page_server(
      id = "treatment_comparisons",
      model = model,
      model_sub = model_sub,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure
    )

    # 3c. Ranking Panel
    ranking_panel_server(
      id = "ranking",
      model = model,
      model_sub = model_sub,
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      exclusions = exclusions,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt
    )

    # 3d. Nodesplit model

    # Inconsistency test with notesplitting model for all studies
    model_nodesplit <- eventReactive(input$node, {
      nodesplit(sub = FALSE, data(), treatment_df(), metaoutcome(), outcome_measure(),
                model_effects(), exclusions())
    })

    output$node_table<- renderTable(colnames=TRUE, {
      model_nodesplit()
    })

    # Inconsistency test with notesplitting model with studies excluded
    model_nodesplit_sub <- eventReactive(input$node_sub, {
      nodesplit(sub = TRUE, data(), treatment_df(), metaoutcome(), outcome_measure(),
                model_effects(), exclusions())
    })

    output$node_table_sub<- renderTable(colnames=TRUE, {
      model_nodesplit_sub()
    })

    output$downloadnode <- downloadHandler(
      filename = 'Nodesplit.csv',
      content = function(file) {
        write.csv(model_nodesplit(), file)
      }
    )

    output$downloadnode_sub <- downloadHandler(
      filename = 'Nodesplit_sen.csv',
      content = function(file) {
        write.csv(model_nodesplit_sub(), file)
      }
    )

    # 3e. Bayesian result details

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

    # 3f. Deviance report

    # Residual deviance from NMA model and UME inconsistency model for all studies
    umeplot <- eventReactive(model(), {
      scat_plot(model())$p
    })

    output$dev_scat <- renderPlotly({
      umeplot()
    })

    # Residual deviance from NMA model and UME inconsistency model with studies excluded
    umeplot_sub <- eventReactive(model_sub(), {
      scat_plot(model_sub())$p
    })

    output$dev_scat_sub <- renderPlotly({
      umeplot_sub()
    })

    # Per-arm residual deviance for all studies
    output$dev1 <- renderPlotly({
      stemplot(model())
    })

    # Per-arm residual deviance for sensitivity analysis
    output$dev1_sub <- renderPlotly({
      stemplot(model_sub())
    })

    # Leverage plot for all studies
    output$dev2 <- renderPlotly({
      levplot(model())
    })

    output$dev2_sub <- renderPlotly({
      levplot(model_sub())
    })

    # 3g. Model details

    # 3g-1 Model codes
    output$code <- renderPrint({
      cat(model()$mtcResults$model$code, fill=FALSE, labels=NULL, append=FALSE)
    })

    output$download_code <- downloadHandler(
      filename = "code.txt",
      content = function(file){
        file.copy("./codes.txt", file)
      }
    )

    # 3g-2 Initial values
    output$inits <- renderPrint({
      model()$mtcResults$model$inits
    })

    #' Create a download handler for the initial values for a given chain
    #'
    #' @param index the Index of the chain
    #' @return The created download handler
    create_chain_initial_data_download_handler <- function(index) {
      filename <- paste0("initialvalues_chain", index, ".txt")
      return(
        downloadHandler(
          filename = filename,
          content = function(file) {
            lapply(
              model()$mtcResults$model$inits[[index]],
              write,
              file,
              append = TRUE,
              ncolumns=1000
            )
          }
        )
      )
    }

    output$download_inits_1 <- create_chain_initial_data_download_handler(1)
    output$download_inits_2 <- create_chain_initial_data_download_handler(2)
    output$download_inits_3 <- create_chain_initial_data_download_handler(3)
    output$download_inits_4 <- create_chain_initial_data_download_handler(4)

    # 3g-3 Chain data.

    #' Create a download handler for the data for a given chain
    #'
    #' @param index the Index of the chain
    #' @return The created download handler
    create_chain_data_download_handler <- function(index) {
      return(
        downloadHandler(
          filename = paste0("data_for_chain_", index, ".csv"),
          content = function(file) {
            data <- as.data.frame(model()$mtcResults$samples[[index]])
            write.csv(data, file)
          }
        )
      )
    }

    output$download_data1 <- create_chain_data_download_handler(1)
    output$download_data2 <- create_chain_data_download_handler(2)
    output$download_data3 <- create_chain_data_download_handler(3)
    output$download_data4 <- create_chain_data_download_handler(4)

    # 3g-4 Output deviance

    # NMA consistency model (all studies)
    output$dev <- renderPrint({
      mtc.deviance({model()$mtcResults})
    })

    # NMA consistency model (sensitivity)
    output$dev_sub <- renderPrint({
      mtc.deviance({model_sub()$mtcResults})
    })

    # UME inconsistency model (all studies)
    output$dev_ume<- renderPrint({
      scat_plot(model())$y
    })

    # UME inconsistency model (sensitivity)
    output$dev_ume_sub<- renderPrint({
      scat_plot(model_sub())$y
    })
  })
}