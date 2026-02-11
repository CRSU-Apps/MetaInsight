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
    if (is.null(common$configured_data)){
      common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }

    if (nrow(common$configured_data$treatments) < 3 || nrow(common$configured_data$treatments) > 10){
      common$logger |> writeLog(type = "error", "Sorry this module is only available when there are between 3 and 10 treatments")
      return()
    }
    # TRIGGER
    trigger("freq_summary")
  })

  svg_all <- reactive({
    watch("freq_all")
    req(watch("freq_summary") > 0)
    common$meta$freq_summary$used <- TRUE
    freq_summary(common$configured_data,
                 "Summary forest plot for all studies",
                 common$logger)
  })

  svg_sub <- reactive({
    watch("setup_exclude")
    req(watch("freq_summary") > 0)
    if (nrow(common$subsetted_data$treatments) < 3){
      common$logger |> writeLog(type = "error", "Sorry the plot with studies excluded cannot be produced when there are fewer than 3 treatments")
      return()
    }

    freq_summary(common$subsetted_data,
                 "Summary forest plot with selected studies excluded",
                 common$logger)
  })

  output$plot_all <- renderUI({
    req(svg_all())
    svg_container(svg_all(), style = "max-width: 800px;")
  })

  output$plot_sub <- renderUI({
    req(svg_sub())
    svg_container(svg_sub(), style = "max-width: 800px;")
  })

  output$download_all <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_all.", common$download_format)
    },
    content = function(file) {
      write_plot(svg_all(), file, common$download_format)
    }
  )

  output$download_sub <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_sub.", common$download_format)
    },
    content = function(file) {
      write_plot(svg_sub(), file, common$download_format)
    }
  )
})
}

freq_summary_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(align = "center",
      uiOutput(ns("plot_all")),
      uiOutput(ns("plot_sub"))
    )
  )
}

freq_summary_module_rmd <- function(common) {
  list(freq_summary_knit = !is.null(common$meta$freq_summary$used))
}

