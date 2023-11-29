
#' Module UI for the ranking panel
#' 
#' @param id ID of the module
#' @return Div for the panel
ranking_panel_ui <- function(id) {
  ns <- NS(id)
  div(
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
          cellWidths = c("30%", "40%", "30%"),
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
                'PDF' = 'pdf',
                'PNG' = 'png'
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
              condition = "input.rank_plot_choice == 0",
              ns = ns,
              shinycssloaders::withSpinner(
                plotOutput(outputId = ns("Litmus")),
                type = 6
              ),
              p("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")
            ),
            conditionalPanel(
              condition = "input.rank_plot_choice == 1 && !input.Radial_alt",
              ns = ns,
              shinycssloaders::withSpinner(
                plotOutput(outputId = ns("Radial")),
                type = 6
              )
            ),
            conditionalPanel(
              condition = "input.rank_plot_choice == 1 && input.Radial_alt",
              ns = ns,
              shinycssloaders::withSpinner(
                plotOutput(outputId = ns("RadialAlt")),
                type = 6
              )
            ),
            conditionalPanel(
              condition = "input.rank_plot_choice == 1",
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
              inline = TRUE
            ),
            radioButtons(
              inputId = ns('network_rank_choice'),
              label = 'Document format',
              choices = c(
                'PDF' = 'pdf',
                'PNG' = 'png'
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
          cellWidths = c("30%", "40%", "30%"),
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
              choices = list(
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
              inline = TRUE
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
  )
}


#' Module server for the ranking panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param freq_sub Reactive containing frequentist meta-analysis for the sensitivity analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
ranking_panel_server <- function(
    id,
    model,
    model_sub,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    exclusions,
    rank_option,
    freq_all,
    freq_sub,
    bugsnetdt
    ) {
  moduleServer(id, function(input, output, session) {
    # Obtain Data needed for ranking #
    RankingData <- eventReactive(model(), {
      obtain_rank_data(data(), metaoutcome(), treatment_df(), model(), rank_option())
    })

    RankingData_sub <- eventReactive(model_sub(), {
      obtain_rank_data(data(), metaoutcome(), treatment_df(), model_sub(), rank_option(), exclusions())
    })

    # Network plots for ranking panel (Bayesian) (they have slightly different formatting to those on tab1) CRN
    treat_order <- reactive({
      RankingData()$SUCRA[order(RankingData()$SUCRA$SUCRA), 1]
    }) # obtain treatments ordered by SUCRA #
    
    freq_all_react <- eventReactive(model(), {
      # These two lines are needed in case someone jumped to Bayesian page without running frequentist section, but am aware this can cause frequentist analysis to run twice (CRN)
      freq_all()
    })
    
    bugsnetdt_react <- eventReactive(model(), {
      bugsnetdt()
    })
    
    output$netGraphStatic1_rank <- renderPlot({
      if (input$networkstyle_rank == 'networkp1') {
        # Number of trials on line
        make_netgraph_rank(freq_all_react(), treat_order())
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(bugsnetdt_react(), order = list(order = treat_order()))
      }
      title("Network plot of all studies")
    })
    
    # Repeat for excluded studies
    treat_order_sub <- reactive({
      RankingData_sub()$SUCRA[order(RankingData_sub()$SUCRA$SUCRA), 1]
    })
    
    freq_all_react_sub <- eventReactive(model_sub(), {
      freq_sub()
    })
    
    bugsnetdt_react_sub <- eventReactive(model_sub(), {
      bugsnetdt()
    })
    
    output$netGraphStatic1_rank_sub <- renderPlot({
      if (input$networkstyle_rank_sub == 'networkp1') {
        # Number of trials on line
        make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order = list(order = treat_order_sub()))
      }
      title("Network plot with studies excluded")
    })

    output$download_network_rank <- downloadHandler(
      filename = function() {
        paste0('Network.', input$network_rank_choice)
      },
      content = function(file) {
        draw_network_rank <- function() {
          if (input$networkstyle_rank == 'networkp1') {
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
            make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order = list(order = treat_order_sub()))
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
      gemtc::forest(model()$mtcRelEffects, digits = 3)
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
      gemtc::forest(model_sub()$mtcRelEffects, digits = 3)
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
          gemtc::forest(model_sub()$mtcRelEffects, digits = 3)
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
      plots$Litmus <- LitmusRankOGram(CumData = RankingData()$Cumulative, SUCRAData = RankingData()$SUCRA, ColourData = RankingData()$Colour, colourblind = FALSE)
      plots$Radial <- RadialSUCRA(SUCRAData = RankingData()$SUCRA, ColourData = RankingData()$Colour, BUGSnetData = RankingData()$BUGSnetData, colourblind = FALSE)
      plots$Litmus_blind <- LitmusRankOGram(CumData = RankingData()$Cumulative, SUCRAData = RankingData()$SUCRA, ColourData = RankingData()$Colour, colourblind = TRUE)
      plots$Radial_blind <- RadialSUCRA(SUCRAData = RankingData()$SUCRA, ColourData = RankingData()$Colour, BUGSnetData = RankingData()$BUGSnetData, colourblind = TRUE)
      return(plots)
    })
    
    Rankplots_sub <- reactive({
      plots <- list()
      plots$Litmus <- LitmusRankOGram(CumData = RankingData_sub()$Cumulative, SUCRAData = RankingData_sub()$SUCRA, ColourData = RankingData_sub()$Colour, colourblind = FALSE)
      plots$Radial <- RadialSUCRA(SUCRAData = RankingData_sub()$SUCRA, ColourData = RankingData_sub()$Colour, BUGSnetData = RankingData_sub()$BUGSnetData, colourblind = FALSE)
      plots$Litmus_blind <- LitmusRankOGram(CumData = RankingData_sub()$Cumulative, SUCRAData = RankingData_sub()$SUCRA, ColourData = RankingData_sub()$Colour, colourblind = TRUE)
      plots$Radial_blind <- RadialSUCRA(SUCRAData = RankingData_sub()$SUCRA, ColourData = RankingData_sub()$Colour, BUGSnetData = RankingData_sub()$BUGSnetData, colourblind = TRUE)
      return(plots)
    })

    # Litmus Rank-O-Gram
    output$Litmus <- renderPlot({
      if (!input$Colour_blind) {
        Rankplots()$Litmus
      } else {
        Rankplots()$Litmus_blind
      }
    })
    
    output$Litmus_sub <- renderPlot({
      if (!input$Colour_blind_sub) {
        Rankplots_sub()$Litmus
      } else {
        Rankplots_sub()$Litmus_blind
      }
    })

    # Radial SUCRA
    output$Radial <- renderPlot({
      if (!input$Colour_blind) {
        Rankplots()$Radial$Original
      } else {
        Rankplots()$Radial_blind$Original
      }
    })
    
    output$Radial_sub <- renderPlot({
      if (!input$Colour_blind_sub) {
        Rankplots_sub()$Radial$Original
      } else {
        Rankplots_sub()$Radial_blind$Original
      }
    })
    
    # Alternative SUCRA plots
    output$RadialAlt <- renderPlot({
      if (!input$Colour_blind) {
        Rankplots()$Radial$Alternative
      } else {
        Rankplots()$Radial_blind$Alternative
      }
    })
    
    output$RadialAlt_sub <- renderPlot({
      if (!input$Colour_blind_sub) {
        Rankplots_sub()$Radial$Alternative
      } else {
        Rankplots_sub()$Radial_blind$Alternative
      }
    })

    output$download_rank_plot <- downloadHandler(
      filename = function() {
        paste0('Ranking_Allstudies.png')
      },
      content = function(file) {
        if (input$rank_plot_choice == 0) { #Litmus Rank-O-Grams
          if (!input$Colour_blind) {
            ggsave(file, Rankplots()$Litmus, width = 6, height = 6, units = "in")
          } else {
            ggsave(file, Rankplots()$Litmus_blind, width = 6, height = 6, units = "in")
          }
        } else {  # Radial SUCRA plots
          if (!input$Radial_alt) { #Default plot
            if (!input$Colour_blind) {
              ggsave(file, Rankplots()$Radial$Original)
            } else {
              ggsave(file, Rankplots()$Radial_blind$Original)
            }
          } else { # Alternative plot
            if (!input$Colour_blind) {
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
          if (!input$Colour_blind_sub) {
            ggsave(file, Rankplots_sub()$Litmus, width = 6, height = 6, units = "in")
          } else {
            ggsave(file, Rankplots_sub()$Litmus_blind, width = 6, height = 6, units = "in")
          }
        } else {  # Radial SUCRA plots
          if (!input$Radial_alt_sub) { #Default plot
            if (!input$Colour_blind_sub) {
              ggsave(file, Rankplots_sub()$Radial$Original)
            } else {
              ggsave(file, Rankplots_sub()$Radial_blind$Original)
            }
          } else { # Alternative plot
            if (!input$Colour_blind_sub) {
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
      {
        rank_probs_table(RankingData())
      },
      digits = 2,
      rownames = FALSE,
      colnames = TRUE
    )
    output$rank_probs_sub <- renderTable(
      {
        rank_probs_table(RankingData_sub())
      },
      digits = 2,
      rownames = FALSE,
      colnames = TRUE
    )

    output$download_rank_table <- downloadHandler(
      filename = 'RankingTable.csv',
      content = function(file) {
        write.csv(
          RankingData()$Probabilities %>% right_join(RankingData()$SUCRA[, 1:2], by = "Treatment"),
          file,
          row.names = FALSE,
          col.names = TRUE
        )
      }
    )

    output$download_rank_table_sub <- downloadHandler(
      filename = 'RankingTable_Excluded.csv',
      content = function(file) {
        write.csv(
          RankingData_sub()$Probabilities %>% right_join(RankingData_sub()$SUCRA[, 1:2], by = "Treatment"),
          file,
          row.names = FALSE,
          col.names = TRUE
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
  })
}