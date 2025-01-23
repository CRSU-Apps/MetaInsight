
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
#' @param model_valid Reactive containing whether the model is valid.
#' @param filename_prefix Prefix to add before file names.
#' @param title_prefix Prefix to add before plot titles.
ranking_forest_panel_server <- function(
    id,
    model,
    treat_order,
    frequentist_react,
    bugsnetdt_react,
    model_valid,
    filename_prefix,
    title_prefix
    ) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      if (is.null(model_valid()) || !model_valid()) {
        shinyjs::disable(id = "download_rank_forest")
      } else {
        shinyjs::enable(id = "download_rank_forest")
      }
    })

    # Forest plots for ranking panel (different style due to using 'boxes' in UI) CRN
    # All studies #
    output$gemtc2 <- renderPlot({
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
      temp_dir <- tempdir()
      png(file.path(temp_dir, "forest.png"))  # initialise image
      gemtc::forest(model()$mtcRelEffects, digits = 3)
      dev.off()
      ForestImg <- magick::image_read(file.path(temp_dir, 'forest.png'))
      Img <- cowplot::ggdraw() +
        cowplot::draw_image(ForestImg)

      unlink(file.path(temp_dir, 'forest.png'))

      return(Img)
    })

    output$download_rank_forest <- downloadHandler(
      filename = function() {
        paste0(filename_prefix, "Forest.", input$rank_forest_choice)
      },
      content = function(file) {
        draw_forest <- function() {
          gemtc::forest(model()$mtcRelEffects, digits = 3)
          title(paste0(title_prefix, ": Bayesian ", model()$a, " consistency model forest plot results"), cex.main = 0.85)
          if (model()$mtcResults$model$type == 'regression') {
            mtext(model()$cov_value_sentence, side = 1, adj = 0)
          }
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
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
      relative_rank_text(model())
    })
  })
}





#' Module server for the baseline risk ranking panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param treat_order Reactive containing treatments ordered by SUCRA
#' @param bugsnetdt_react Reactive containing bugsnet meta-analysis
#' @param model_valid Reactive containing whether the model is valid.
#' @param filename_prefix Prefix to add before file names.
#' @param title_prefix Prefix to add before plot titles.
ranking_forest_panel_baseline_risk_server <- function(
    id,
    model,
    treat_order,
    bugsnetdt_react,
    model_valid,
    filename_prefix,
    title_prefix
) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      if (is.null(model_valid()) || !model_valid()) {
        shinyjs::disable(id = "download_rank_forest")
      } else {
        shinyjs::enable(id = "download_rank_forest")
      }
    })
    
    # Forest plots for ranking panel (different style due to using 'boxes' in UI) CRN
    # All studies #
    output$gemtc2 <- renderPlot({
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
      temp_dir <- tempdir()
      png(file.path(temp_dir, "forest.png"))  # initialise image
      bnma::network.forest.plot(model(), only.reference.treatment = TRUE)
      dev.off()
      ForestImg <- magick::image_read(file.path(temp_dir, 'forest.png'))
      Img <- cowplot::ggdraw() +
        cowplot::draw_image(ForestImg)

      unlink(file.path(temp_dir, 'forest.png'))

      return(Img)
    })
    
    output$download_rank_forest <- downloadHandler(
      filename = function() {
        paste0(filename_prefix, "forest.", input$rank_forest_choice)
      },
      content = function(file) {
        if (input$rank_forest_choice == "pdf"){
          pdf(file = file, height = BayesInch(model()$network$ntreat))
        } else  if (input$rank_forest_choice == "png"){
          png(file = file, height = BayesPixels(model()$network$ntreat))
        }
        bnma::network.forest.plot(model(), only.reference.treatment = TRUE)
        dev.off()
      }
    )
    
  })
}