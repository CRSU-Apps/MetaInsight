freq_summary_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    div(class = "freq_summary_div",
      download_button_pair(id)
    )
  )
}

freq_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  hide_and_show("freq_summary")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }

    if (nrow(common$treatment_df) < 3 || nrow(common$treatment_df) > 10){
      common$logger |> writeLog(type = "error", "Sorry this module is only available when there are between 3 and 10 treatments")
      return()
    }
    # TRIGGER
    trigger("freq_summary")
  })

  svg_all <- reactive({
    watch("model")
    req(watch("freq_summary") > 0)
    common$meta$freq_summary$used <- TRUE
    freq_summary(common$freq_all,
                 common$treatment_df,
                 "Summary forest plot for all studies",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type,
                 common$seed,
                 common$logger)
  })

  svg_sub <- reactive({
    watch("setup_exclude")
    req(watch("freq_summary") > 0)
    if (nrow(common$subsetted_treatment_df) < 3){
      common$logger |> writeLog(type = "error", "Sorry the plot with studies excluded cannot be produced when there are fewer than 3 treatments")
      return()
    }

    freq_summary(common$freq_sub,
                 common$subsetted_treatment_df,
                 "Summary forest plot with selected studies excluded",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type,
                 common$seed,
                 common$logger)
  })

  output$plot_all <- renderUI({
    req(svg_all())
    div(class = "svg_container",
        tags$button(
          class = "height-toggle-btn",
          onclick = "shinyjs.scrollingPlot(this)",
          "↕ Allow Scrolling"
        ),
        tags$button(
          class = "fullscreen-btn",
          onclick = "shinyjs.fullscreenPlot(this.parentElement)",
          "⤢"
        ),
        HTML(svg_all()$svg)
    )
  })

  output$plot_sub <- renderUI({
    req(svg_sub())
    div(class = "svg_container",
        HTML(svg_sub()$svg)
        )
  })

  output$download_all <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_all.", common$download_format)
    },
    content = function(file) {
      write_svg_plot(file, common$download_format, svg_all())
    }
  )

  output$download_sub <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_sub.", common$download_format)
    },
    content = function(file) {
      write_svg_plot(file, common$download_format, svg_sub())
    }
  )
})
}

freq_summary_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(align = "center",
      div(style = "max-width: 700px;",
          uiOutput(ns("plot_all"))),
      div(style = "max-width: 700px;",
          uiOutput(ns("plot_sub")))
    )
  )
}

freq_summary_module_rmd <- function(common) {
  list(freq_summary_knit = !is.null(common$meta$freq_summary$used))
}

