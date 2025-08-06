freq_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    div(class = "freq_forest_div",
      download_button_pair(id),
      fixedRow(
        p("Limits of the x-axis for all studies:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_all"), label = "Minimum", value = 0, step = 0.1),
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_all"), label = "Maximum", value = 5, step = 0.1),
        )
      ),
      fixedRow(
        p("Limits of the x-axis without excluded studies:"),
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
      )
    )
  )
}

freq_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide(selector = ".freq_forest_div")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type = "error", "Please configure the analysis first in the Setup component")
      return()
    }
    # TRIGGER
    trigger("freq_forest")
  })

  observe({
    watch("model")
    req(watch("freq_forest") > 0)
    ci <- extract_ci(common$freq_all, common$outcome)
    updateNumericInput(session, "xmin_all", value = ci$xmin)
    updateNumericInput(session, "xmax_all", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_sub", min = 0.01, step = 0.01)
    }
  })

  observe({
    watch("setup_exclude")
    req(watch("freq_forest") > 0)
    ci <- extract_ci(common$freq_sub, common$outcome)
    updateNumericInput(session, "xmin_sub", value = ci$xmin)
    updateNumericInput(session, "xmax_sub", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_sub", min = 0.01, step = 0.01)
    }
  })

  result_all <- reactive({
    watch("model")
    req(watch("freq_forest") > 0)
    common$meta$freq_forest$used <- TRUE
    common$meta$freq_forest$xmin_all <- as.numeric(input$xmin_all)
    common$meta$freq_forest$xmax_all <- as.numeric(input$xmax_all)
    shinyjs::show(selector = ".freq_forest_div")

    freq_forest(common$freq_all,
                common$reference_treatment_all,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_all),
                as.numeric(input$xmax_all))
  })

  result_sub <- reactive({
    watch("setup_exclude")
    req(watch("freq_forest") > 0)
    common$meta$freq_forest$xmin_sub <- as.numeric(input$xmin_sub)
    common$meta$freq_forest$xmax_sub <- as.numeric(input$xmax_sub)

    freq_forest(common$freq_sub,
                common$reference_treatment_sub,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_sub),
                as.numeric(input$xmax_sub))
  })


  output$plot_sub <- renderUI({
    height <- (result_sub()$height_pixels) / 72 # divide by 72 dpi
    common$meta$freq_forest$height_sub <- height
    # make width responsive to treatment label
    common$meta$freq_forest$width_sub <- 5 + (max(nchar(common$treatment_df$Label)) / 10)

    svg_text <- svglite::xmlSVG(
      {result_sub()$plot()
        grid::grid.text("Results with selected studies excluded", 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
        grid::grid.text(result_sub()$annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))},
      height = height,
      width = common$meta$freq_forest$width_sub)

    div(class = "svg_container",
        HTML(paste(svg_text, collapse = "\n"))
    )
  })

  output$plot_all <- renderUI({
    height <- (result_all()$height_pixels) / 72 # divide by 72 dpi
    common$meta$freq_forest$height_all <- height
    # make width responsive to treatment label
    common$meta$freq_forest$width_all <- 5 + (max(nchar(common$treatment_df$Label)) / 10)

    svg_text <- svglite::xmlSVG(
      {result_all()$plot()
       grid::grid.text("Results for all studies", 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
       grid::grid.text(result_all()$annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))},
      height = height,
      width = common$meta$freq_forest$width_all)

    div(class = "svg_container",
        HTML(paste(svg_text, collapse = "\n"))
    )
  })

  output$download_all <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_all.", common$download_format)},
    content = function(file){

      height <- common$meta$freq_forest$height_all
      width <- common$meta$freq_forest$width_all

      if (common$download_format == "pdf"){
        pdf(file = file, height = height, width = width)
      }
      if (common$download_format == "png"){
        png(file = file, height = height, width = width, units = "in", res = 300)
      }
      if (common$download_format == "svg"){
        svg(file = file, height = height, width = width)
      }
      result_all()$plot()
      grid::grid.text("Results for all studies", 0.5, grid::unit(height - 0.25, "inches"), gp=grid::gpar(cex=1.2, fontface = "bold"))
      grid::grid.text(result_all()$annotation, 0.5, grid::unit(height - 0.65, "inches"), gp=grid::gpar(cex=1))
      dev.off()
    }
  )

  output$download_sub <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_sub.", common$download_format)},
    content = function(file){

      height <- common$meta$freq_forest$height_sub
      width <- common$meta$freq_forest$width_sub

      if (common$download_format == "pdf"){
        pdf(file = file, height = height, width = width)
      }
      if (common$download_format == "png"){
        png(file = file, height = height, width = width, units = "in", res = 300)
      }
      if (common$download_format == "svg"){
        svg(file = file, height = height, width = width)
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
  layout_columns(
    uiOutput(ns("plot_all")),
    uiOutput(ns("plot_sub"))
  )
}

freq_forest_module_rmd <- function(common){ list(
  freq_forest_knit = !is.null(common$meta$freq_forest$used),
  freq_forest_xmin_all = common$meta$freq_forest$xmin_all,
  freq_forest_xmax_all = common$meta$freq_forest$xmax_all,
  freq_forest_xmin_sub = common$meta$freq_forest$xmin_sub,
  freq_forest_xmax_sub = common$meta$freq_forest$xmax_sub,
  freq_forest_height_all = common$meta$freq_forest$height_all,
  freq_forest_width_all = common$meta$freq_forest$width_all,
  freq_forest_height_sub = common$meta$freq_forest$height_sub,
  freq_forest_width_sub = common$meta$freq_forest$width_sub)
}

