freq_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    conditionalPanel("input.run > 0", download_button_pair(id), ns = ns)
  )
}

freq_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger %>% writeLog(type = "error", "Please define the data first in the Setup component")
      return()
    }
    # TRIGGER
    trigger("freq_summary")
  })

  output$plot_all <- renderPlot({
    watch("model")
    req(watch("freq_summary") > 0)
    common$meta$freq_summary$used <- TRUE
    common$meta$freq_summary$height <- 2.5 * nrow(common$treatment_df)
    common$meta$freq_summary$width <- 2.5 * nrow(common$treatment_df)
    freq_summary(common$freq_all,
                 common$treatment_df,
                 "Summary Forest Plot",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type,
                 common$logger)
  })

  output$plot_sub <- renderPlot({
    watch("summary_exclude")
    req(watch("freq_summary") > 0)
    freq_summary(common$freq_sub,
                 common$treatment_df,
                 "Summary Forest Plot with Selected Studies Excluded",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type,
                 common$logger)
  })

  output$download_all <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_all.", common$download_format)
    },
    content = function(file) {
      write_plot(
        file = file,
        type = common$download_format,
        renderFunction = function() {
          freq_summary(common$freq_all,
                       common$treatment_df,
                       "Summary Forest Plot",
                       common$outcome_measure,
                       common$ranking_option,
                       common$model_type)
        },
        height = common$meta$freq_summary$height,
        width = common$meta$freq_summary$width
      )
    }
  )

  output$download_sub <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_sub.", common$download_format)
    },
    content = function(file) {
      write_plot(
        file = file,
        type = common$download_format,
        renderFunction = function() {
          freq_summary(common$freq_sub,
                       common$treatment_df,
                       "Summary Forest Plot",
                       common$outcome_measure,
                       common$ranking_option,
                       common$model_type)
        },
        height = common$meta$freq_summary$height,
        width = common$meta$freq_summary$width
      )
    }
  )
})
}

freq_summary_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot_all"), height = "700px"),
    plotOutput(ns("plot_sub"), height = "700px")
  )
}

freq_summary_module_rmd <- function(common) {
  list(freq_summary_knit = !is.null(common$meta$freq_summary$used),
       freq_summary_height = common$meta$freq_summary$height,
       freq_summary_width = common$meta$freq_summary$width)
}

