freq_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(

    actionButton(ns("run"), "Run module freq_summary"),

    radioButtons(inputId = ns("format_all"),
                 label = "All studies format",
                 choices = c("PDF", "PNG"),
                 inline = TRUE),
    downloadButton(outputId = ns('download_all')),

    radioButtons(inputId = ns("format_sub"),
                 label = "Without excluded studies format",
                 choices = c("PDF", "PNG"),
                 inline = TRUE),
    downloadButton(outputId = ns('download_sub'))
  )
}

freq_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    gargoyle::trigger("freq_summary")
  })

  output$plot_all <- renderPlot({
    watch("setup_define")
    watch("model")
    req(common$freq_all)
    freq_summary(common$freq_all,
                 common$treatment_df,
                 "Summary Forest Plot",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type)
  })

  output$plot_sub <- renderPlot({
    watch("summary_exclude")
    req(common$freq_sub)
    freq_summary(common$freq_sub,
                 common$treatment_df,
                 "Summary Forest Plot with Selected Studies Excluded",
                 common$outcome_measure,
                 common$ranking_option,
                 common$model_type)
  })

  output$download_all <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_all.", tolower(input$format_all))
    },
    content = function(file) {
      write_to_pdf_or_png(
        file = file,
        type = input$format_all,
        renderFunction = function() {
          freq_summary(common$freq_all,
                       common$treatment_df,
                       "Summary Forest Plot",
                       common$outcome_measure,
                       common$ranking_option,
                       common$model_type)
        },
        height = 2.5 * nrow(common$treatment_df),
        width = 2.5 * nrow(common$treatment_df)
      )
    }
  )

  output$download_sub <- downloadHandler(
    filename = function() {
      paste0("MetaInsight_summary_forest_sub.", tolower(input$format_sub))
    },
    content = function(file) {
      write_to_pdf_or_png(
        file = file,
        type = input$format_sub,
        renderFunction = function() {
          freq_summary(common$freq_sub,
                       common$treatment_df,
                       "Summary Forest Plot",
                       common$outcome_measure,
                       common$ranking_option,
                       common$model_type)
        },
        height = 2.5 * nrow(common$treatment_df),
        width = 2.5 * nrow(common$treatment_df)
      )
    }
  )


  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      # Populate using save_and_load()
    },
    load = function(state) {
      # Load
      # Populate using save_and_load()
    }
  ))
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
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

