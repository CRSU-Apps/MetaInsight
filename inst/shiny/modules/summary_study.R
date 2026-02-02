summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    div(class = "summary_study_div download_buttons",
       checkboxInput(ns("colourblind"), "Use colourblind palette", FALSE),
       numericInput(ns("width"), "Plot area width:", value = 6, step = 1, min = 6, max = 20),
       p("Limits of the x-axis:"),
       numericInput(ns("x_min"), "Minimum:", value = 0),
       numericInput(ns("x_max"), "Maximum:", value = 0),
       downloadButton(ns("download")),
    )
  )
}

summary_study_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  hide_and_show(id)

  # setup inputs
  observe({
    watch("setup_exclude")
    req(common$subsetted_data)

    # only show colourblind option if ROB exist
    if (length(metainsight:::FindRobNames(common$subsetted_data)) == 0){
      shinyjs::hide("colourblind")
    } else {
      shinyjs::show("colourblind")
    }

    min_max <- summary_study_min_max(common$freq_sub$d1, common$outcome)
    if (common$outcome == "Binary"){
      min_max <- exp(min_max)
    }
    updateNumericInput(session, "x_min", value = min_max[1], step = format_step(min_max[1]))
    updateNumericInput(session, "x_max", value = min_max[2], step = format_step(min_max[2]))

  })

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type= "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }
    # TRIGGER
    trigger("summary_study")
  })

  svg <- reactive({
    watch("setup_exclude")
    req(watch("summary_study") > 0)

    if (common$outcome == "Binary"){
      x_min = log(as.numeric(input$x_min))
      x_max = log(as.numeric(input$x_max))
    } else {
      x_min = as.numeric(input$x_min)
      x_max = as.numeric(input$x_max)
    }

    common$meta$summary_study$used <- TRUE
    common$meta$summary_study$width <- as.numeric(input$width)
    common$meta$summary_study$colourblind <- input$colourblind
    common$meta$summary_study$x_min <- x_min
    common$meta$summary_study$x_max <- x_max

    summary_study(
      connected_data = common$subsetted_data,
      freq = common$freq_sub,
      outcome_measure = common$outcome_measure,
      plot_area_width = as.numeric(input$width),
      colourblind = input$colourblind,
      x_min = x_min,
      x_max = x_max,
      common$logger
    )
  })

  output$plot <- renderUI({
    req(svg())
    div(
      class = "svg_container",
      svg()
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0('MetaInsight_study_results.', common$download_format)
    },
    content = function(file) {
      write_plot(
        svg(),
        file,
        common$download_format
      )
    }
  )

  return(
    list(
      save = function() {
        list(
          ### Manual save start
          ### Manual save end
          colourblind = input$colourblind,
          width = input$width,
          x_min = input$x_min,
          x_max = input$x_max
          )
        },
      load = function(state) {
        ### Manual load start
        ### Manual load end
        updateCheckboxInput(session, "colourblind", value = state$colourblind)
        updateNumericInput(session, "width", value = state$width)
        updateNumericInput(session, "x_min", value = state$x_min)
        updateNumericInput(session, "x_max", value = state$x_max)
      }
    )
  )
  })
}

summary_study_module_result <- function(id) {
  ns <- NS(id)
  # set a maximum width whilst staying centered
  div(style = "max-width: 1200px; margin: 0 auto;", uiOutput(ns("plot")))
}

summary_study_module_rmd <- function(common) {
  list(
    summary_study_knit = !is.null(common$meta$summary_study$used),
    summary_study_width = common$meta$summary_study$width,
    summary_study_colourblind = common$meta$summary_study$colourblind,
    summary_study_x_min = common$meta$summary_study$x_min,
    summary_study_x_max = common$meta$summary_study$x_max
  )
}

