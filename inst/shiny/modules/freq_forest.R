freq_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    actionButton(ns("run_all"), "Run all modules", icon = icon("forward-fast")),
    div(class = "freq_forest_div",
      fixedRow(
        p("Limits of the x-axis for all studies:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_all"), label = "Minimum", value = 0, step = 0.1),
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_all"), label = "Maximum", value = 5, step = 0.1),
        )
      ),
      fixedRow(
        p("Limits of the x-axis without excluded studies:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_sub"), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_sub"), label = "Maximum", value = 5, step = 0.1)
        )
      ),
      download_button_pair(id)
    )
  )
}

freq_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  hide_and_show("freq_forest")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                 "Please configure the analysis first in the Setup section")
      return()
    }
    # TRIGGER
    trigger("freq_forest")
  })

  observe({
    watch("model")
    req(watch("freq_forest") > 0)
    ci <- extract_ci(common$freq_all, common$outcome)
    updateNumericInput(session, "xmin_all", value = ci$xmin)
    updateNumericInput(session, "xmax_all", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_all", min = 0.01, step = 0.01)
    }
  })

  observe({
    watch("setup_exclude")
    req(watch("freq_forest") > 0)
    ci <- extract_ci(common$freq_sub, common$outcome)
    updateNumericInput(session, "xmin_sub", value = ci$xmin)
    updateNumericInput(session, "xmax_sub", value = ci$xmax)

    # prevent errors when set to 0
    if (common$outcome == "Binary"){
      updateNumericInput(session, "xmin_sub", min = 0.01, step = 0.01)
    }
  })

  result_all <- reactive({
    watch("model")
    req(watch("freq_forest") > 0)
    common$meta$freq_forest$used <- TRUE
    common$meta$freq_forest$xmin_all <- as.numeric(input$xmin_all)
    common$meta$freq_forest$xmax_all <- as.numeric(input$xmax_all)
    # shinyjs::show(selector = ".freq_forest_div")

    freq_forest(common$freq_all,
                common$reference_treatment_all,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_all),
                as.numeric(input$xmax_all),
                "Results for all studies")
  })

  result_sub <- reactive({
    watch("setup_exclude")
    req(watch("freq_forest") > 0)
    common$meta$freq_forest$xmin_sub <- as.numeric(input$xmin_sub)
    common$meta$freq_forest$xmax_sub <- as.numeric(input$xmax_sub)

    freq_forest(common$freq_sub,
                common$reference_treatment_sub,
                common$model_type,
                common$outcome_measure,
                as.numeric(input$xmin_sub),
                as.numeric(input$xmax_sub),
                "Results with selected studies excluded")
  })

  output$plot_sub <- renderUI({
    div(class = "svg_container",
        result_sub()
    )
  })

  output$plot_all <- renderUI({
    div(class = "svg_container",
        result_all()
    )
  })

  output$download_all <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_all.", common$download_format)},
    content = function(file){
      write_plot(result_all(), file, common$download_format)
    }
  )

  output$download_sub <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_frequentist_forest_sub.", common$download_format)},
    content = function(file){
      write_plot(result_sub(), file, common$download_format)
    }
  )

  observeEvent(input$run_all, {
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }
    run_all(COMPONENTS, COMPONENT_MODULES, "freq", common$logger)
  })

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

freq_forest_module_result <- function(id) {
  ns <- NS(id)
  layout_columns(
    uiOutput(ns("plot_all")),
    uiOutput(ns("plot_sub"))
  )
}

freq_forest_module_rmd <- function(common){ list(
  freq_forest_knit = !is.null(common$meta$freq_forest$used),
  freq_forest_xmin_all = common$meta$freq_forest$xmin_all,
  freq_forest_xmax_all = common$meta$freq_forest$xmax_all,
  freq_forest_xmin_sub = common$meta$freq_forest$xmin_sub,
  freq_forest_xmax_sub = common$meta$freq_forest$xmax_sub)
}

