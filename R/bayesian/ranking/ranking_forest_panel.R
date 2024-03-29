
#' Module UI for the forest plot in the ranking panel
#' 
#' @param id ID of the module
#' @return Div for the panel
ranking_forest_panel_ui <- function(id) {
  ns <- NS(id)
  div(
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
  )
}


#' Module server for the ranking panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param treat_order Reactive containing treatments ordered by SUCRA
#' @param frequentist_react Reactive containing frequentist meta-analysis
#' @param bugsnetdt_react Reactive containing bugsnet meta-analysis
#' @param filename_prefix Prefix to add before file names.
#' @param title_prefix Prefix to add beofre plot titles.
ranking_forest_panel_server <- function(
    id,
    model,
    treat_order,
    frequentist_react,
    bugsnetdt_react,
    filename_prefix,
    title_prefix
    ) {
  moduleServer(id, function(input, output, session) {

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

    output$download_rank_forest <- downloadHandler(
      filename = function() {
        paste0(filename_prefix, ".", input$rank_forest_choice)
      },
      content = function(file) {
        draw_forest <- function() {
          gemtc::forest(model()$mtcRelEffects, digits = 3)
          title(paste0(title_prefix, ": Bayesian ", model()$a, " consistency model forest plot results"), cex.main = 0.85)
        }
        write_to_pdf_or_png(
          file,
          input$rank_forest_choice,
          draw_forest
        )
      }
    )
    
    # Text underneath
    output$relative_rank_text <- renderText({
      relative_rank_text(model())
    })
  })
}