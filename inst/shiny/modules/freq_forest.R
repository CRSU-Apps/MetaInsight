freq_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Run module freq_forest")
  )
}

freq_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observe({
    watch("setup_define")
    watch("model")
    req(common$freq_all)
    ci <- extract_ci(common$freq_all, common$outcome)
    updateNumericInput(session, "xmin_all", value = ci$xmin)
    updateNumericInput(session, "xmax_all", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_sub", min = 0.01, step = 0.01)
    }
  })

  observe({
    watch("summary_exclude")
    req(common$freq_sub)
    ci <- extract_ci(common$freq_sub, common$outcome)
    updateNumericInput(session, "xmin_sub", value = ci$xmin)
    updateNumericInput(session, "xmax_sub", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_sub", min = 0.01, step = 0.01)
    }
  })


  result_all <- reactive({
    watch("setup_define")
    watch("model")
    req(common$freq_all)

    common$meta$freq_forest$used <- TRUE
    common$meta$freq_forest$xmin_all <- as.numeric(input$xmin_all)
    common$meta$freq_forest$xmax_all <- as.numeric(input$xmax_all)

    freq_forest(common$freq_all,
                common$reference_treatment_all,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_all),
                as.numeric(input$xmax_all))
  })

  output$plot_all <- renderPlot({

    req(result_all())
    result_all()$plot()
    title("Results for all studies")
    mtext(result_all()$annotation, padj = 0.5)
  })


  result_sub <- reactive({
    watch("summary_exclude")
    req(common$freq_sub)
    common$meta$freq_forest$xmin_sub <- as.numeric(input$xmin_sub)
    common$meta$freq_forest$xmax_sub <- as.numeric(input$xmax_sub)

    freq_forest(common$freq_sub,
                common$reference_treatment_sub,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_sub),
                as.numeric(input$xmax_sub))
  })

  output$plot_sub <- renderPlot({
    req(result_sub())
    result_sub()$plot()
    title("Results with selected studies excluded")
    mtext(result_sub()$annotation, padj = 0.5)
  })

  output$plot_wrap_all <- renderUI({
    req(result_all())
    plotOutput(session$ns("plot_all"), height = result_all()$height_pixels)
  })

  output$plot_wrap_sub <- renderUI({
    req(result_sub())
    plotOutput(session$ns("plot_sub"), height = result_sub()$height_pixels)
  })

  output$download_all <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_all.", tolower(input$format_all))},
    content = function(file){

      # remove title, divide by 72 dpi
      height <- (result_all()$height_pixels) / 72

      # make width responsive to treatment label
      width <- 5 + (max(nchar(common$treatment_df$Label)) / 10)

      if (input$format_all == "PDF"){
        pdf(file = file, height = height, width = width)
      } else {
        png(file = file, height = height, width = width, units = "in", res = 300)
      }
      result_all()$plot()
      grid::grid.text("Results for all studies", 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
      grid::grid.text(result_all()$annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))
      dev.off()
    }
  )

  output$download_sub <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_sub.", tolower(input$format_sub))},
    content = function(file){

      # divide by 72 dpi
      height <- (result_sub()$height_pixels) / 72

      # make width responsive to treatment label
      width <- 5 + (max(nchar(common$treatment_df$Label)) / 10)

      if (input$format_sub == "PDF"){
        pdf(file = file, height = height, width = width)
      } else {
        png(file = file, height = height, width = width, units = "in", res = 300)
      }
      result_sub()$plot()
      grid::grid.text("Results with selected studies excluded", 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
      grid::grid.text(result_sub()$annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))
      dev.off()
    }
  )

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      xmin_all = input$xmin_all,
      xmax_all = input$xmax_all,
      xmin_sub = input$xmin_sub,
      xmax_sub = input$xmax_sub)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "xmin_all", value = state$xmin_all)
      updateNumericInput(session, "xmax_all", value = state$xmax_all)
      updateNumericInput(session, "xmin_sub", value = state$xmin_sub)
      updateNumericInput(session, "xmax_sub", value = state$xmax_sub)
    }
  ))
})
}

freq_forest_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      uiOutput(ns("plot_wrap_all")),
      fixedRow(
        p("Options to change limits of the x-axis:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_all"), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_all"), label = "Maximum", value = 5, step = 0.1)
        )
      ),
      radioButtons(
        ns("format_all"),
        label = "Document format",
        choices = c("PDF", "PNG"),
        inline = TRUE
      ),
      downloadButton(ns("download_all"))
    ),
    column(
      width = 6,
      uiOutput(ns("plot_wrap_sub")),
      fixedRow(
        p("Options to change limits of the x-axis:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_sub"), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_sub"), label = "Maximum", value = 5, step = 0.1)
        )
      ),
      radioButtons(
        ns("format_sub"),
        label = "Document format",
        choices = c("PDF", "PNG"),
        inline = TRUE
      ),
      downloadButton(ns("download_sub"))
    )
  )

}

freq_forest_module_rmd <- function(common){ list(
  freq_forest_knit = !is.null(common$meta$freq_forest$used),
  freq_forest_xmin_all = common$meta$freq_forest$xmin_all,
  freq_forest_xmax_all = common$meta$freq_forest$xmax_all,
  freq_forest_xmin_sub = common$meta$freq_forest$xmin_sub,
  freq_forest_xmax_sub = common$meta$freq_forest$xmax_sub)
}

