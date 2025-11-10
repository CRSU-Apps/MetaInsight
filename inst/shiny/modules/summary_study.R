summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    div(class = "summary_study_div",
       checkboxInput(ns("colourblind"), "Use colourblind palette", FALSE),
       numericInput(ns("width"), "Plot area width:", value = 6, step = 1, min = 6, max = 20),
       numericInput(ns("x_min"), "x-axis min:", value = 0, step = 0.1),
       numericInput(ns("x_max"), "x-axis max:", value = 0, step = 0.1),
       downloadButton(ns("download")),
    )
  )
}

summary_study_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide(selector = ".summary_study_div")

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

  svg <- reactive({
    watch("setup_exclude")
    req(watch("summary_study") > 0)
    summary_study(
      data = common$main_connected_data,
      freq = common$freq_sub,
      outcome_measure = common$outcome_measure,
      plot_area_width = input$width,
      colourblind = input$colourblind,
      x_min = ifelse(input$x_min == 0, NA, input$x_min),
      x_max = ifelse(input$x_max == 0, NA, input$x_max)
    )
  })

  # listen for default values for the axis but only after the initial run
  # or when setup_configure is rerun
  observe({
    watch("setup_configure")
    min_max$resume()
  })

  min_max <- observe({
    updateNumericInput(session, "x_min", value = svg()$x_min, step = svg()$step)
    updateNumericInput(session, "x_max", value = svg()$x_max, step = svg()$step)
    if (is.element(common$outcome_measure, c("MD", "SMD", "RD"))) {
      updateNumericInput(session, "x_min", label = "x-axis min:")
      updateNumericInput(session, "x_max", label = "x-axis max:")
    } else if (is.element(common$outcome_measure, c("OR", "RR"))) {
      updateNumericInput(session, "x_min", label = "x-axis min (log scale):")
      updateNumericInput(session, "x_max", label = "x-axis max (log scale):")
    }
    min_max$suspend()
  })

  output$plot <- renderUI({
    common$meta$summary_study$used <- TRUE
    common$meta$summary_study$width <- as.numeric(input$width)
    common$meta$summary_study$colourblind <- input$colourblind
    common$meta$summary_study$x_min <- as.numeric(input$x_min)
    common$meta$summary_study$x_max <- as.numeric(input$x_max)
    div(
      class = "svg_container",
      HTML(svg()$svg)
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0('MetaInsight_study_results.', common$download_format)
    },
    content = function(file) {
      write_svg_plot(
        file,
        common$download_format,
        svg()$svg,
        svg()$height,
        svg()$width
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

