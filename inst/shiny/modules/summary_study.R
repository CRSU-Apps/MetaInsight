summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    div(class = "summary_study_div",
       numericInput(ns("title"), label = "Title text size:", value = 1, step = 0.1),
       numericInput(ns("header"), label = "Group headers text size:", value = 1, step = 0.1),
       numericInput(ns("x_axis_min"), label = "x-axis min:", value = 1, step = 0.1),
       numericInput(ns("x_axis_max"), label = "x-axis max:", value = 10, step = 0.1),
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

  # Determine the plot height in pixels


  # #Set default values for the axes
  # observe({
  #   starting_x_limits <- FindStartingXLimits(pairwise = pairwise())
  #   updateNumericInput(inputId = "x_axis_min", value = unname(starting_x_limits["lower"]))
  #   updateNumericInput(inputId = "x_axis_max", value = unname(starting_x_limits["upper"]))
  # })

  svg <- reactive({
    req(watch("summary_study") > 0)
    summary_study(
      data = common$data,
      freq = common$freq_sub,
      outcome_measure = common$outcome_measure,
      x_limits = c(lower = input$x_axis_min, upper = input$x_axis_max)
    )
  })

  output$plot <- renderUI({
    common$meta$summary_study$used <- TRUE
    common$meta$summary_study$title <- as.numeric(input$title)
    common$meta$summary_study$header <- as.numeric(input$header)
    # common$meta$summary_study$height <- plot_height()/72 # pixels to inches
    div(class = "svg_container",
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
  div(style = "max-width: 1200px; margin: 0 auto;", uiOutput(ns("plot")))
}

summary_study_module_rmd <- function(common){ list(
  summary_study_knit = !is.null(common$meta$summary_study$used),
  summary_study_title = common$meta$summary_study$title,
  summary_study_header = common$meta$summary_study$header,
  summary_study_height = common$meta$summary_study$height)
}

