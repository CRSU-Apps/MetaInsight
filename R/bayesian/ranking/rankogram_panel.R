
#' Module UI for the rankogram panel.
#' 
#' @param id ID of the module.
#' @param table_label Label for the drop-down to show ranking values and SUCRA values.
#' @return Div for the panel.
rankogram_panel_ui <- function(id, table_label) {
  ns <- NS(id)
  div(
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
        label = table_label
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
  )
}


#' Module server for the rankogram panel.
#' 
#' @param id ID of the module.
#' @param ranking_data Reactive containing ranking data.
#' @param filename_prefix Prefix to add before file names.
rankogram_panel_server <- function(
    id,
    ranking_data,
    filename_prefix
    ) {
  moduleServer(id, function(input, output, session) {

    # All rank plots in one function for easier loading when switching options #
    Rankplots <- reactive({
      plots <- list()
      plots$Litmus <- LitmusRankOGram(CumData = ranking_data()$Cumulative, SUCRAData = ranking_data()$SUCRA, ColourData = ranking_data()$Colour, colourblind = FALSE)
      plots$Radial <- RadialSUCRA(SUCRAData = ranking_data()$SUCRA, ColourData = ranking_data()$Colour, BUGSnetData = ranking_data()$BUGSnetData, colourblind = FALSE)
      plots$Litmus_blind <- LitmusRankOGram(CumData = ranking_data()$Cumulative, SUCRAData = ranking_data()$SUCRA, ColourData = ranking_data()$Colour, colourblind = TRUE)
      plots$Radial_blind <- RadialSUCRA(SUCRAData = ranking_data()$SUCRA, ColourData = ranking_data()$Colour, BUGSnetData = ranking_data()$BUGSnetData, colourblind = TRUE)
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

    # Radial SUCRA
    output$Radial <- renderPlot({
      if (!input$Colour_blind) {
        Rankplots()$Radial$Original
      } else {
        Rankplots()$Radial_blind$Original
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

    output$download_rank_plot <- downloadHandler(
      filename = paste0(filename_prefix, 'Ranking.png'),
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

    # Table of Probabilities (need to include SUCRA and have it as a collapsable table)
    output$rank_probs <- renderTable(
      {
        rank_probs_table(ranking_data())
      },
      digits = 2,
      rownames = FALSE,
      colnames = TRUE
    )

    output$download_rank_table <- downloadHandler(
      filename = paste0(filename_prefix, 'RankingTable.csv'),
      content = function(file) {
        write.csv(
          ranking_data()$Probabilities %>% right_join(ranking_data()$SUCRA[, 1:2], by = "Treatment"),
          file,
          row.names = FALSE,
          col.names = TRUE
        )
      }
    )
  })
}