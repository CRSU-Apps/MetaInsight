summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    div(class = "summary_study_div",
       numericInput(ns("title"), label = "Title text size:", value = 1, step = 0.1),
       numericInput(ns("header"), label = "Group headers text size:", value = 1, step = 0.1),
       downloadButton(ns("download")),
    )
  )
}

summary_study_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type= "error", "Please configure the analysis first in the Setup section")
      return()
    }
    # TRIGGER
    trigger("summary_study")
    shinyjs::show(selector = ".summary_study_div")
  })

  # Determine the plot height in pixels
  plot_height <- reactive({
    watch("setup_exclude")
    req(watch("summary_study") > 0)
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
    watch("setup_exclude")
    req(watch("summary_study") > 0)
    common$meta$summary_study$used <- TRUE
    common$meta$summary_study$title <- as.numeric(input$title)
    common$meta$summary_study$header <- as.numeric(input$header)
    common$meta$summary_study$height <- plot_height()/72 # pixels to inches
    summary_study(common$freq_sub, common$outcome_measure, as.numeric(input$header), as.numeric(input$title))
  }, height = function(){plot_height()})

  output$download <- downloadHandler(
    filename = function() {
      paste0('MetaInsight_study_results.', common$download_format)
    },
    content = function(file) {

      write_plot(file,
                          common$download_format,
                          function(){summary_study(common$freq_sub, common$outcome_measure, as.numeric(input$header), as.numeric(input$title))},
                          width = 8,
                          height = common$meta$summary_study$height
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
  # set a maximum width whilst staying centered
  div(style = "max-width: 800px; margin: 0 auto;", plotOutput(ns("plot")))
}

summary_study_module_rmd <- function(common){ list(
  summary_study_knit = !is.null(common$meta$summary_study$used),
  summary_study_title = common$meta$summary_study$title,
  summary_study_header = common$meta$summary_study$header,
  summary_study_height = common$meta$summary_study$height)
}

