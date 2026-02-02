metaregression_forest_module_ui <- function(id, parent_id) {
  ns <- NS(id)
  div(class = glue("{parent_id}_div download_buttons"),
      downloadButton(ns("download"), "Download plot")
  )
}

covariate_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "covariate_forest_div",
      p("Limits of the x-axis:"),
      numericInput(ns("xmin"), "Minimum", 0),
      numericInput(ns("xmax"), "Maximum", 0)
    ),
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    metaregression_forest_module_ui(ns("covariate"), id)
  )
}

metaregression_forest_module_server <- function(id, common, run, xmin, xmax) {
  moduleServer(id, function(input, output, session) {

    module_id <- glue("{id}_forest")
    model <- glue("{id}_model")

    hide_and_show(module_id)

    observeEvent(run(), {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", go_to = model, glue("Please fit the {id} model first"))
        return()
      } else {
        common$meta[[module_id]]$used <- TRUE
        trigger(module_id)
      }
    })

    svg <- reactive({
      watch(glue("{model}_fit"))
      req(watch(module_id) > 0)

      common$meta[[module_id]]$xmin <- xmin()
      common$meta[[module_id]]$xmax <- xmax()

      if (id == "covariate"){
        plot_title  <- "Covariate regression analysis"
      } else {
        plot_title  <- "Baseline risk regression analysis"
      }

      do.call(module_id, list(common[[model]],
                           common$treatment_df,
                           xmin = xmin(),
                           xmax = xmax(),
                           title = plot_title
                           ))
    })

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container", style = "max-width: 800px;",
          svg()
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        glue("MetaInsight_{id}_forest_plot.{common$download_format}")
      },
      content = function(file) {
        write_plot(svg(), file, common$download_format)
      }
    )

  })
}

covariate_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # update xlim inputs
    observe({
      watch("covariate_model_fit")
      req(common$covariate_model)

      limits <- bayes_forest_limits(common$covariate_model, common$reference_treatment_all)

      if (common$outcome == "Binary"){
        limits <- exp(limits)
      }

      updateNumericInput(session, "xmin", value = limits[1], step = format_step(limits[1]))
      updateNumericInput(session, "xmax", value = limits[2], step = format_step(limits[2]))

    })

    # convert values back to log when outcome is Binary
    xmin <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmin)), as.numeric(input$xmin)))
    xmax <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmax)), as.numeric(input$xmax)))

    metaregression_forest_module_server("covariate", common, reactive(input$run), xmin, xmax)

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
        xmin = input$xmin,
        xmax = input$xmax)
      },
      load = function(state) {
        ### Manual load start
        ### Manual load end
        updateNumericInput(session, "xmin", value = state$xmin)
        updateNumericInput(session, "xmax", value = state$xmax)
      }
    ))

  })
}


metaregression_forest_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      uiOutput(ns("plot"))
  )
}

covariate_forest_module_result <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_result(ns("covariate"))
}


covariate_forest_module_rmd <- function(common) {
  list(covariate_forest_knit = !is.null(common$meta$covariate_forest$used),
       covariate_forest_xmin = common$meta$covariate_forest$xmin,
       covariate_forest_xmax = common$meta$covariate_forest$xmax
       )
}

