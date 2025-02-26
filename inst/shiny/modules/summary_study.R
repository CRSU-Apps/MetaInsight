summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("title"), label = "Title text size:", value = 1, step = 0.1),
    numericInput(ns("header"), label = "Group headers text size:", value = 1, step = 0.1),
    downloadButton(ns("download"))
  )
}

summary_study_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(list(watch("summary_exclude"), input$content, input$title, input$header), {
    req(common$freq_sub)

    # METADATA ####
      common$meta$summary_study$used <- TRUE
      common$meta$summary_study$content <- as.numeric(input$content)
      common$meta$summary_study$title <- as.numeric(input$title)
      common$meta$summary_study$header <- as.numeric(input$header)
      common$meta$summary_study$format <- input$format

    # TRIGGER
    trigger("summary_study")
  })

  # Determine the plot height in pixels
  plot_height <- reactive({
    watch("summary_study")
    req(common$freq_sub)
    # The rows that don't correspond to NA treatment effects
    proper_comparison_rows <- !is.na(common$freq_sub$d0$TE)
    # The number of comparisons, with NA rows dropped
    n_proper_comparisons <- length(common$freq_sub$d0$TE[proper_comparison_rows])
    # The number of unique treatments, with NA rows dropped
    n_proper_treatments <- length(unique(c(common$freq_sub$d0$treat1[proper_comparison_rows],
                                           common$freq_sub$d0$treat2[proper_comparison_rows])))
    return(n_proper_comparisons * 25 + n_proper_treatments * 35)
  })

  output$plot <- renderPlot({
    watch("summary_study")
    req(common$freq_sub)
    summary_study(common$freq_sub, common$outcome_measure, as.numeric(input$header), as.numeric(input$title))
  }, height = function(){plot_height()})

  output$download <- downloadHandler(
    filename = function() {
      paste0('MetaInsight_study_results.', common$download_format)
    },
    content = function(file) {

      write_to_pdf_or_png(file,
                          common$download_format,
                          function(){summary_study(common$freq_sub, common$outcome_measure, as.numeric(input$header), as.numeric(input$title))},
                          width = 8,
                          height = plot_height()/72 #pixels to inches
                          )
      }
  )

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      title = input$title,
      header = input$header,
      format = input$format)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "title", value = state$title)
      updateNumericInput(session, "header", value = state$header)
      updateRadioButtons(session, "format", selected = state$format)
    }
  ))
})
}

summary_study_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

summary_study_module_rmd <- function(common){ list(
  summary_study_knit = !is.null(common$meta$summary_study$used),
  summary_study_title = common$meta$summary_study$title,
  summary_study_header = common$meta$summary_study$header)
}

