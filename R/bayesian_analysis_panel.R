
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
        helpText(
          "Please note: if you change the selections on the sidebar,
          you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page.",
          tags$br(),
          tags$strong("Please note it may take up to 5 minutes to load the results.", style = "color:#FF0000"),
          tags$br(),
          tags$strong(
            "IMPORTANT: If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:",
            style = "color:#4863A0"
          ),
          tags$a(
            href = "https://doi.org/10.1016/j.jclinepi.2023.02.016",
            "Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed to aid interpretation of network meta-analysis,
            Journal of Clinical Epidemiology (2023)"
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Ranking panel for all studies",
            status = 'primary',
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            splitLayout(
              cellWidths = c("30%","40%","30%"),
              cellArgs = list(style = "height: 780px; padding: 16px; border: 2px solid gold; white-space: normal"),
              fluidRow(
                align = "center",
                h4("Relative Effects"),
                shinycssloaders::withSpinner(
                  plotOutput(outputId = ns("gemtc2")),
                  type = 6
                ),
                textOutput(outputId = ns("relative_rank_text")),
                radioButtons(
                  inputId = ns('rank_forest_choice'),
                  label = 'Document format',
                  choices = c(
                    'PDF'='pdf',
                    'PNG'='png'
                  ),
                  inline = TRUE
                ),
                downloadButton(outputId = ns('download_rank_forest'))
              ),
              fluidRow(
                align = "center",
                h4("Ranking Results"),
                radioButtons(
                  inputId = ns("rank_plot_choice"),
                  label = "Choice of Rank plot",
                  choices = list(
                    "Litmus Rank-O-Gram" = 0,
                    "Radial SUCRA" = 1
                  ),
                  selected = 0,
                  inline = TRUE
                ),
                checkboxInput(
                  inputId = ns("Colour_blind"),
                  label = "Display colour-blind friendly version",
                  value = FALSE
                ),
                conditionalPanel(
                  condition= "input.rank_plot_choice == 0",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("Litmus")),
                    type = 6
                  ),
                  p("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")
                ),
                conditionalPanel(
                  condition="input.rank_plot_choice == 1 && !input.Radial_alt",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("Radial")),
                    type = 6
                  )
                ),
                conditionalPanel(
                  condition="input.rank_plot_choice == 1 && input.Radial_alt",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("RadialAlt")),
                    type = 6
                  )
                ),
                conditionalPanel(
                  condition="input.rank_plot_choice == 1",
                  ns = ns,
                  checkboxInput(inputId = ns("Radial_alt"), label = "Display simplified version", value = FALSE),
                  p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")
                ),
                shinyWidgets::dropMenu(
                  shinyWidgets::dropdownButton(
                    circle = FALSE,
                    status = 'warning',
                    label = "Ranking probabilities and SUCRA values for all treatments"
                  ),
                  tableOutput(outputId = ns("rank_probs"))
                ),
                div(style = "margin-bottom:10px"), #some padding between buttons
                fluidRow(
                  column(
                    width = 6,
                    downloadButton(
                      outputId = ns('download_rank_plot'),
                      label = "Download Rank plot (PNG)",
                      style = "width:220px; white-space: normal"
                    )
                  ),
                  column(
                    width = 6,
                    downloadButton(
                      outputId = ns('download_rank_table'),
                      label = "Download table of rank probablities and SUCRA",
                      style = "width:220px; white-space: normal"
                    )
                  )
                )
              ),
              fluidRow(
                align = "center",
                h4("Summary of evidence"),
                shinycssloaders::withSpinner(
                  plotOutput(outputId = ns("netGraphStatic1_rank")),
                  type = 6
                ),
                conditionalPanel(
                  condition = "input.networkstyle_rank == 'networkp1'",
                  ns = ns,
                  p("Numbers on the line indicate number of trials conducted for the comparison. Any shaded areas indicate existence of multi-arm trials between the comparisons.")
                ),
                conditionalPanel(
                  condition = "input.networkstyle_rank == 'networkp2'",
                  ns = ns,
                  p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                ),
                radioButtons(
                  inputId = ns("networkstyle_rank"),
                  label = "Please choose a network plot style",
                  choices = c(
                    "Number of trials shown on the line" = "networkp1",
                    "Number of trials indicated by node size and line thickness" = "networkp2"
                  ),
                  inline=TRUE
                ),
                radioButtons(
                  inputId = ns('network_rank_choice'),
                  label = 'Document format',
                  choices = c(
                    'PDF'='pdf',
                    'PNG'='png'
                  ),
                  inline = TRUE
                ),
                downloadButton(outputId = ns('download_network_rank'))
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Ranking panel with studies excluded",
            status = 'primary',
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            splitLayout(
              cellWidths = c("30%","40%","30%"),
              cellArgs = list(style = "height: 780px; padding: 16px; border: 2px solid gold; white-space: normal"),
              fluidRow(
                align = "center",
                h4("Relative Effects"),
                shinycssloaders::withSpinner(
                  plotOutput(outputId = ns("gemtc_sub2")),
                  type = 6
                ),
                textOutput(outputId = ns("relative_rank_text_sub")),
                radioButtons(
                  inputId = ns('rank_forest_choice_sub'),
                  label = 'Document format',
                  choices = c(
                    'PDF' = 'pdf',
                    'PNG' = 'png'
                  ),
                  inline = TRUE
                ),
                downloadButton(outputId = ns('download_rank_forest_sub'))
              ),
              fluidRow(
                align = "center",
                h4("Ranking Results"),
                radioButtons(
                  inputId = ns("rank_plot_choice_sub"),
                  label = "Choice of Rank plot",
                  choices= list(
                    "Litmus Rank-O-Gram" = 0,
                    "Radial SUCRA" = 1
                  ),
                  selected = 0,
                  inline = TRUE
                ),
                checkboxInput(inputId = ns("Colour_blind_sub"), label = "Display colour-blind friendly version", value = FALSE),
                conditionalPanel(
                  condition = "input.rank_plot_choice_sub == 0",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("Litmus_sub")),
                    type = 6
                  ),
                  p("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")
                ),
                conditionalPanel(
                  condition = "input.rank_plot_choice_sub == 1 && !input.Radial_alt_sub",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("Radial_sub")),
                    type = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.rank_plot_choice_sub == 1 && input.Radial_alt_sub",
                  ns = ns,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("RadialAlt_sub")),
                    type = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.rank_plot_choice_sub == 1",
                  ns = ns,
                  checkboxInput(
                    inputId = ns("Radial_alt_sub"),
                    label = "Display simplified version",
                    value = FALSE
                  ),
                  p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")
                ),
                shinyWidgets::dropMenu(
                  shinyWidgets::dropdownButton(
                    circle = FALSE,
                    status = 'warning',
                    label = "Ranking probabilities and SUCRA values for all treatments"
                  ),
                  tableOutput(outputId = ns("rank_probs_sub"))
                ),
                div(style = "margin-bottom:10px"), #some padding between buttons
                fluidRow(
                  column(
                    width = 6,
                    downloadButton(
                      outputId = ns('download_rank_plot_sub'),
                      label = "Download Rank plot (PNG)",
                      style = "width:220px; white-space: normal"
                    )
                  ),
                  column(
                    width = 6,
                    downloadButton(
                      outputId = ns('download_rank_table_sub'),
                      label = "Download table of rank probablities and SUCRA",
                      style = "width:220px; white-space: normal"
                    )
                  )
                )
              ),
              fluidRow(
                align = "center",
                h4("Summary of evidence"),
                shinycssloaders::withSpinner(
                  plotOutput(outputId = ns("netGraphStatic1_rank_sub")),
                  type = 6
                ),
                conditionalPanel(
                  condition = "input.networkstyle_rank_sub == 'networkp1'",
                  ns = ns,
                  p("Numbers on the line indicate number of trials conducted for the comparison. Any shaded areas indicate existence of multi-arm trials between the comparisons.")
                ),
                conditionalPanel(
                  condition = "input.networkstyle_rank_sub == 'networkp2'",
                  ns = ns,
                  p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                ),
                radioButtons(
                  inputId = ns("networkstyle_rank_sub"),
                  label = "Please choose a network plot style",
                  choices = c(
                    "Number of trials shown on the line" = "networkp1",
                    "Number of trials indicated by node size and line thickness" = "networkp2"
                  ),
                  inline=TRUE
                ),
                radioButtons(
                  inputId = ns('network_rank_choice_sub'),
                  label = 'Document format',
                  choices = c(
                    'PDF' = 'pdf',
                    'PNG' = 'png'
                  ),
                  inline = TRUE
                ),
                downloadButton(outputId = ns('download_network_rank_sub'))
              )
            )
          )
        )
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

    # Obtain Data needed for ranking #
    RankingData <- eventReactive(model(), {
      obtain_rank_data(data(), metaoutcome(),
                       treatment_df(), model(), rank_option())
    })

    RankingData_sub <- eventReactive(model_sub(), {
      obtain_rank_data(data(), metaoutcome(), treatment_df(),
                       model_sub(), rank_option(), exclusions())
    })

    # Network plots for ranking panel (Bayesian) (they have slightly different formatting to those on tab1) CRN
    treat_order <- reactive(RankingData()$SUCRA[order(RankingData()$SUCRA$SUCRA),1]) # obtain treatments ordered by SUCRA #
    freq_all_react <- eventReactive(model(), {
      # These two lines are needed in case someone jumped to Bayesian page without running frequentist section, but am aware this can cause frequentist analysis to run twice (CRN)
      freq_all()
    })
    bugsnetdt_react <- eventReactive(model(), {
      bugsnetdt()
    })
    output$netGraphStatic1_rank <- renderPlot({
      if (input$networkstyle_rank=='networkp1') {
        # Number of trials on line
        make_netgraph_rank(freq_all_react(), treat_order())
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(bugsnetdt_react(), order=list(order=treat_order()))
      }
      title("Network plot of all studies")
    })
    # Repeat for excluded studies
    treat_order_sub <- reactive(RankingData_sub()$SUCRA[order(RankingData_sub()$SUCRA$SUCRA),1])
    freq_all_react_sub <- eventReactive(model_sub(), {
      freq_sub()
    })
    bugsnetdt_react_sub <- eventReactive(model_sub(), {
      bugsnetdt()
    })
    output$netGraphStatic1_rank_sub <- renderPlot({
      if (input$networkstyle_rank_sub=='networkp1') {
        # Number of trials on line
        make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order=list(order=treat_order_sub()))
      }
      title("Network plot with studies excluded")
    })

    output$download_network_rank <- downloadHandler(
      filename = function() {
        paste0('Network.', input$network_rank_choice)
      },
      content = function(file) {
        draw_network_rank <- function() {
          if (input$networkstyle_rank =='networkp1') {
            make_netgraph_rank(freq_all_react(), treat_order())
          } else {
            make_netplot(bugsnetdt_react(), order = list(order = treat_order()))
          }
          title("Network plot of all studies")
        }
        write_to_pdf_or_png(
          file,
          input$network_rank_choice,
          draw_network_rank
        )
      }
    )

    output$download_network_rank_sub <- downloadHandler(
      filename = function() {
        paste0('Network_sen.', input$network_rank_choice_sub)
      },
      content = function(file) {
        draw_network_rank <- function() {
          if (input$networkstyle_rank_sub == 'networkp1') {
            make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
          } else {
            make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order = list(order=treat_order_sub()))
          }
          title("Network plot with studies excluded")
        }
        write_to_pdf_or_png(
          file,
          input$network_rank_choice_sub,
          draw_network_rank
        )
      }
    )

    # Forest plots for ranking panel (different style due to using 'boxes' in UI) CRN
    # All studies #
    output$gemtc2 <- renderPlot({
      png("forest.png")  # initialise image
      gemtc::forest(model()$mtcRelEffects,digits=3)
      dev.off()
      ForestImg <- magick::image_read('forest.png')
      Img <- cowplot::ggdraw() +
        cowplot::draw_image(ForestImg)

      file.remove('forest.png')

      return(Img)
    })
    # With studies excluded
    output$gemtc_sub2 <- renderPlot({
      png("forest_sub.png")
      gemtc::forest(model_sub()$mtcRelEffects,digits=3)
      dev.off()
      ForestImg <- magick::image_read('forest_sub.png')
      Img <- cowplot::ggdraw() +
        cowplot::draw_image(ForestImg)

      file.remove('forest_sub.png')

      return(Img)
    })

    output$download_rank_forest <- downloadHandler(
      filename = function() {
        paste0('All_studies.', input$rank_forest_choice)
      },
      content = function(file) {
        draw_forest <- function() {
          gemtc::forest(model()$mtcRelEffects, digits = 3)
          title(paste("All studies:
                Bayesian", model()$a, "consistency model forest plot results"), cex.main = 0.85)
        }
        write_to_pdf_or_png(
          file,
          input$rank_forest_choice,
          draw_forest
        )
      }
    )

    output$download_rank_forest_sub <- downloadHandler(
      filename = function() {
        paste0('Subgroup.', input$rank_forest_choice_sub)
      },
      content = function(file) {
        draw_forest <- function() {
          gemtc::forest(model_sub()$mtcRelEffects,digits=3)
          title(paste("Results with studies excluded:
                Bayesian", model()$a, "consistency model forest plot results"), cex.main = 0.85)
        }
        write_to_pdf_or_png(
          file,
          input$rank_forest_choice_sub,
          draw_forest
        )
      }
    )

    # All rank plots in one function for easier loading when switching options #
    Rankplots <- reactive({
      plots <- list()
      plots$Litmus <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=FALSE)
      plots$Radial <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, BUGSnetData=RankingData()$BUGSnetData, colourblind=FALSE)
      plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=TRUE)
      plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, BUGSnetData=RankingData()$BUGSnetData, colourblind=TRUE)
      plots
    })
    Rankplots_sub <- reactive({
      plots <- list()
      plots$Litmus <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=FALSE)
      plots$Radial <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, BUGSnetData=RankingData_sub()$BUGSnetData, colourblind=FALSE)
      plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=TRUE)
      plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, BUGSnetData=RankingData_sub()$BUGSnetData, colourblind=TRUE)
      plots
    })

    # Litmus Rank-O-Gram
    output$Litmus <- renderPlot({
      if (input$Colour_blind==FALSE) {Rankplots()$Litmus} else {Rankplots()$Litmus_blind}
    })
    output$Litmus_sub <- renderPlot({
      if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Litmus} else {Rankplots_sub()$Litmus_blind}
    })

    # Radial SUCRA
    output$Radial <- renderPlot({
      if (input$Colour_blind==FALSE) {Rankplots()$Radial$Original} else {Rankplots()$Radial_blind$Original}
    })
    output$Radial_sub <- renderPlot({
      if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Radial$Original} else {Rankplots_sub()$Radial_blind$Original}
    })
    # Alternative SUCRA plots
    output$RadialAlt <- renderPlot({
      if (input$Colour_blind==FALSE) {Rankplots()$Radial$Alternative} else {Rankplots()$Radial_blind$Alternative}
    })
    output$RadialAlt_sub <- renderPlot({
      if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Radial$Alternative} else {Rankplots_sub()$Radial_blind$Alternative}
    })

    output$download_rank_plot <- downloadHandler(
      filename = function() {
        paste0('Ranking_Allstudies.png')
      },
      content = function(file) {
        if (input$rank_plot_choice == 0) { #Litmus Rank-O-Grams
          if (input$Colour_blind == FALSE) {
            ggsave(file, Rankplots()$Litmus, width = 6, height = 6, units = "in")
          } else {
            ggsave(file, Rankplots()$Litmus_blind, width = 6, height = 6, units = "in")
          }
        } else {  # Radial SUCRA plots
          if (input$Radial_alt == FALSE) { #Default plot
            if (input$Colour_blind == FALSE) {
              ggsave(file, Rankplots()$Radial$Original)
            } else {
              ggsave(file, Rankplots()$Radial_blind$Original)
            }
          } else { # Alternative plot
            if (input$Colour_blind == FALSE) {
              ggsave(file, Rankplots()$Radial$Alternative)
            } else {
              ggsave(file, Rankplots()$Radial_blind$Alternative)
            }
          }
        }
      }
    )

    output$download_rank_plot_sub <- downloadHandler(
      filename = function() {
        paste0('Ranking_Excludedstudies.png')
      },
      content = function(file) {
        if (input$rank_plot_choice_sub == 0) { #Litmus Rank-O-Grams
          if (input$Colour_blind_sub == FALSE) {
            ggsave(file, Rankplots_sub()$Litmus, width = 6, height = 6, units = "in")
          } else {
            ggsave(file, Rankplots_sub()$Litmus_blind, width = 6, height = 6, units = "in")
          }
        } else {  # Radial SUCRA plots
          if (input$Radial_alt_sub == FALSE) { #Default plot
            if (input$Colour_blind_sub == FALSE) {
              ggsave(file, Rankplots_sub()$Radial$Original)
            } else {
              ggsave(file, Rankplots_sub()$Radial_blind$Original)
            }
          } else { # Alternative plot
            if (input$Colour_blind_sub == FALSE) {
              ggsave(file, Rankplots_sub()$Radial$Alternative)
            } else {
              ggsave(file, Rankplots_sub()$Radial_blind$Alternative)
            }
          }
        }
      }
    )

    # Table of Probabilities (need to include SUCRA and have it as a collapsable table)
    output$rank_probs <- renderTable(
      {rank_probs_table(RankingData())},
      digits=2, rownames=FALSE, colnames=TRUE)
    output$rank_probs_sub <- renderTable(
      {rank_probs_table(RankingData_sub())},
      digits=2, rownames=FALSE, colnames=TRUE)

    output$download_rank_table <- downloadHandler(
      filename = 'RankingTable.csv',
      content = function(file) {
        write.csv(
          RankingData()$Probabilities %>% right_join(RankingData()$SUCRA[,1:2], by = "Treatment"),
          file,
          row.names=FALSE,
          col.names=TRUE
        )
      }
    )

    output$download_rank_table_sub <- downloadHandler(
      filename = 'RankingTable_Excluded.csv',
      content = function(file) {
        write.csv(
          RankingData_sub()$Probabilities %>% right_join(RankingData_sub()$SUCRA[,1:2], by = "Treatment"),
          file,
          row.names=FALSE,
          col.names=TRUE
        )
      }
    )

    # Text underneath
    output$relative_rank_text <-renderText({
      relative_rank_text(model())
    })
    output$relative_rank_text_sub <-renderText({
      relative_rank_text(model_sub())
    })


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