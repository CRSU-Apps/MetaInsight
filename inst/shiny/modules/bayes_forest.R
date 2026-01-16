bayes_forest_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  downloadButton(ns("download"), download_label)
}

bayes_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # these need to be in the main module for saving and loading.
    div(class = "bayes_forest_div",
      layout_columns(
        col_widths = rep(6, 6),  # six items: 2 per row
        row_heights = c("auto", "auto", "auto"),  # three rows
        p("Limits of the x-axis for all studies:"),
        p("Limits of the x-axis with selected studies excluded:"),
        numericInput(ns("xmin_all"), "Minimum", 0),
        numericInput(ns("xmin_sub"), "Minimum", 0),
        numericInput(ns("xmax_all"), "Maximum", 0),
        numericInput(ns("xmax_sub"), "Maximum", 0),
      )
    ),
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    div(class = "bayes_forest_div download_buttons",
      layout_columns(
        bayes_forest_submodule_ui(ns("all"), "All studies"),
        bayes_forest_submodule_ui(ns("sub"), "Selected studies excluded")
      )
    )
  )
}

bayes_forest_submodule_server <- function(id, common, model, run, xmin, xmax, title){
  moduleServer(id, function(input, output, session) {

    svg <- reactive({
      tdf <- ifelse(id == "all", "treatment_df", "subsetted_treatment_df")

      common$meta$bayes_forest[[paste0("xmin_", id)]] <- xmin()
      common$meta$bayes_forest[[paste0("xmax_", id)]] <- xmax()

      bayes_forest(common[[paste0("bayes_", id)]],
                   common[[tdf]],
                   common[[paste0("reference_treatment_", id)]],
                   xmin(),
                   xmax(),
                   title)
    }) |> bindEvent(run())

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container",
          svg()
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        glue("MetaInsight_bayesian_forest_plot_{id}.{common$download_format}")
      },
      content = function(file) {
        write_plot(svg(),
                   file,
                   common$download_format
                   )

      }
    )

  })
}

bayes_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id)

    # update xlim inputs
    observe({
      watch("bayes_model_all")
      watch("bayes_model_sub")

      req(common$bayes_all, common$bayes_sub)

      all_limits <- bayes_forest_limits(common$bayes_all, common$reference_treatment_all)
      sub_limits <- bayes_forest_limits(common$bayes_sub, common$reference_treatment_sub)

      if (common$outcome == "Binary"){
        all_limits <- exp(all_limits)
        sub_limits <- exp(sub_limits)
        min_step <- 0.01
        max_step <- 1
      } else {
        min_step <- 0.1
        max_step <- 0.1
      }

      updateNumericInput(session, "xmin_all", value = all_limits[1], step = min_step)
      updateNumericInput(session, "xmax_all", value = all_limits[2], step = max_step)
      updateNumericInput(session, "xmin_sub", value = sub_limits[1], step = min_step)
      updateNumericInput(session, "xmax_sub", value = sub_limits[2], step = max_step)

    })

    # check that a fitted model exists and error if not
    observeEvent(input$run, {
      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_forest$used <- TRUE
        trigger("bayes_forest")
      }
    })

    # convert values back to log when outcome is Binary
    xmin_all <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmin_all)), as.numeric(input$xmin_all)))
    xmax_all <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmax_all)), as.numeric(input$xmax_all)))
    xmin_sub <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmin_sub)), as.numeric(input$xmin_sub)))
    xmax_sub <- reactive(ifelse(common$outcome == "Binary", log(as.numeric(input$xmax_sub)), as.numeric(input$xmax_sub)))

    # trigger for the main analysis - when run is clicked or x limits change, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        return(list(watch("bayes_forest"), watch("bayes_model_all"), xmin_all(), xmax_all()))
      }
    })

    # trigger for the sub analysis - when run is clicked, x limits change or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        return(list(watch("bayes_forest"), watch("bayes_model_sub"), xmin_sub(), xmax_sub()))
      }
    })

    bayes_forest_submodule_server("all", common, "bayes_all", all_trigger, xmin_all, xmax_all, "All studies")
    bayes_forest_submodule_server("sub", common, "bayes_sub", sub_trigger, xmin_sub, xmax_sub, "With selected studies excluded")

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
        xmin_all = input$xmin_all,
        xmax_all = input$xmax_all,
        xmin_sub = input$xmin_sub,
        xmax_sub = input$xmax_sub)
      },
      load = function(state) {
        ### Manual load start
        ### Manual load end
        updateNumericInput(session, "xmin_all", value = state$xmin_all)
        updateNumericInput(session, "xmax_all", value = state$xmax_all)
        updateNumericInput(session, "xmin_sub", value = state$xmin_sub)
        updateNumericInput(session, "xmax_sub", value = state$xmax_sub)
      }
    ))
  })
}

bayes_forest_submodule_result <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plot"))
  )
}

bayes_forest_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        align = "center",
        bayes_forest_submodule_result(ns("all"))
      ),
      column(
        width = 6,
        align = "center",
        bayes_forest_submodule_result(ns("sub"))
      )
    )
  )
}


bayes_forest_module_rmd <- function(common) {
  list(bayes_forest_knit = !is.null(common$meta$bayes_forest$used),
       bayes_forest_xmin_all = common$meta$bayes_forest$xmin_all,
       bayes_forest_xmax_all = common$meta$bayes_forest$xmax_all,
       bayes_forest_xmin_sub = common$meta$bayes_forest$xmin_sub,
       bayes_forest_xmax_sub = common$meta$bayes_forest$xmax_sub
      )
}







