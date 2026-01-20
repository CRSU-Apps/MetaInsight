baseline_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "baseline_forest_div",
      p("Limits of the x-axis:"),
      numericInput(ns("xmin"), "Minimum", 0),
      numericInput(ns("xmax"), "Maximum", 0)
    ),
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    metaregression_forest_module_ui(ns("baseline"), id)
  )
}

baseline_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # update xlim inputs
    observe({
      watch("baseline_model_fit")
      req(common$baseline_model)

      median_ci_table <- bnma::relative.effects.table(common$baseline_model$mtcResults, summary_stat = "ci")
      forest_data <- format_baseline_forest(median_ci_table, common$reference_treatment_all)

      limits <- baseline_forest_limits(forest_data)

      if (common$outcome == "Binary"){
        min_step <- 0.01
        max_step <- 1
      } else {
        min_step <- 0.1
        max_step <- 0.1
      }

      updateNumericInput(session, "xmin", value = limits[1], step = min_step)
      updateNumericInput(session, "xmax", value = limits[2], step = max_step)

    })

    xmin <- reactive(as.numeric(input$xmin))
    xmax <- reactive(as.numeric(input$xmax))

    metaregression_forest_module_server("baseline", common, reactive(input$run), xmin, xmax)

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

baseline_forest_module_result <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_result(ns("baseline"))
}

baseline_forest_module_rmd <- function(common) {
  list(baseline_forest_knit = !is.null(common$meta$baseline_forest$used),
       baseline_forest_xmin = common$meta$baseline_forest$xmin,
       baseline_forest_xmax = common$meta$baseline_forest$xmax)
}

