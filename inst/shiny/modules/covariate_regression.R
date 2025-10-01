covariate_regression_module_ui <- function(id) {

  add_tooltip <- function(label, message){
    span(label, tooltip(icon("circle-question"), message))
  }

  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("comparators"), "Treatments to include", choices = c(), multiple = TRUE),
    # Covariate value
    checkboxInput(ns("covariate"),
      label = add_tooltip("Show covariate value?", "Show the covariate value as a vertical line at the current value"),
      value = TRUE
    ),
    # Confidence regions
    checkboxInput(ns("credible"),
      label = add_tooltip("Show credible regions?",
                          "Show credible regions for the added comparisons. This will not show if there is one or zero direct contributions"),
      value = TRUE
    ),
    conditionalPanel("input.credible", ns = ns,
      sliderInput(ns("credible_opacity"),
        label = "Credible region opacity",
        min = 0,
        max = 0.5,
        step = 0.01,
        value = 0.2
      ),
    ),
    # Regression lines
    checkboxInput(ns("ghosts"),
      label = add_tooltip("Show ghost comparisons?", "Show other available comparisons in light grey behind the main plot"),
      value = TRUE
    ),
    checkboxInput(ns("extrapolate"),
      label = add_tooltip("Extrapolate regression lines?", "Extrapolate the regression lines beyond the range of the original data"),
      value = TRUE
    ),
    selectInput(ns("symbol"),
      label = add_tooltip("Covariate symbol", "Show covariate values using a symbol of your choice"),
      choices = c(
        "Nothing" = "none",
        "Circles" = "circle open",
        "Crosses" = "cross"
      ),
      selected = "circle open",
      selectize = FALSE
    ),
    numericInput(ns("symbol_size"),
      label = "Covariate symbol size",
      min = 0,
      value = 10,
      step = 1),
    selectInput(ns("legend_position"),
      label = "Legend position",
      choices = c(
        "Bottom-Right" = "BR",
        "Bottom-Left" = "BL",
        "Top-Right" = "TR",
        "Top-Left" = "TL"
      ),
      selectize = FALSE
    ),
    input_task_button(ns("run"), "Generate plot", type = "default", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download plot")
  )
}

covariate_regression_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide("download")
  init("covariate_regression_plot")

  observe({
    watch("setup_configure")
    req(common$reference_treatment_all)
    all_treatments <- unique(common$treatment_df$Label)
    treatments <- all_treatments[all_treatments != common$reference_treatment_all]
    shinyWidgets::updatePickerInput(session, "comparators", choices = treatments, selected = treatments)
  })

  common$tasks$covariate_regression <- ExtendedTask$new(
    function(...) mirai::mirai(run(...), run = covariate_regression, .args = environment())
  ) |> bind_task_button("run")

  observeEvent(input$run, {
    if (is.null(common$covariate_model)){
      common$logger |> writeLog(type = "error", "Please fit the covariate model first")
      return()
    } else {
      trigger("covariate_regression")
    }
  })

  observeEvent(list(watch("covariate_model"), watch("covariate_regression")), {
    # trigger if run is pressed or if model is changed, but only if a model exists
    req((watch("covariate_regression") > 0 || all(!is.null(common$covariate_model), watch("covariate_model") > 0)))

    if (is.null(common$covariate_regression)){
      common$logger |> writeLog(type = "starting", "Calculating regression plot data")
    } else {
      common$logger |> writeLog(type = "starting", "Updating regression plot data")
    }
    common$tasks$covariate_regression$invoke(common$covariate_model,
                                            common$main_connected_data,
                                            common$covariate_column,
                                            common$covariate_name,
                                            common$treatment_df,
                                            common$outcome,
                                            common$outcome_measure,
                                            common$model_type,
                                            async = TRUE)
    regress_result$resume()
  })


  regress_result <- observe({

    result <- common$tasks$covariate_regression$result()
    regress_result$suspend()
    if (inherits(result, "list")){
      common$covariate_regression <- result
      shinyjs::runjs("Shiny.setInputValue('covariate_regression-complete', 'complete');")
      common$logger |> writeLog(type = "complete", "Regression plot data has been calculated")
    } else {
      common$logger |> writeLog(type = "error", result)
    }

    trigger("covariate_regression_plot")
  })

  output$plot <- renderPlot({
    watch("covariate_regression_plot")
    req(common$covariate_regression)
    on.exit(shinyjs::show("download"))

    # METADATA ####
    common$meta$covariate_regression$used <- TRUE
    common$meta$covariate_regression$comparators <- input$comparators
    common$meta$covariate_regression$covariate <- input$covariate
    common$meta$covariate_regression$credible <- input$credible
    common$meta$covariate_regression$credible_opacity <- as.numeric(input$credible_opacity)
    common$meta$covariate_regression$ghosts <- input$ghosts
    common$meta$covariate_regression$extrapolate <- input$extrapolate
    common$meta$covariate_regression$symbol <- input$symbol
    common$meta$covariate_regression$symbol_size <- as.numeric(input$symbol_size)
    common$meta$covariate_regression$legend_position <- input$legend_position

    covariate_regression_plot(model_output = common$covariate_model,
                              treatment_df = common$treatment_df,
                              outcome_measure = common$outcome_measure,
                              comparators = input$comparators,
                              directness = common$covariate_regression$directness,
                              credible_regions = common$covariate_regression$credible_regions,
                              include_covariate = input$covariate,
                              include_ghosts = input$ghosts,
                              include_extrapolation = input$extrapolate,
                              include_credible = input$credible,
                              credible_opacity = input$credible_opacity,
                              covariate_symbol = input$symbol,
                              covariate_symbol_size = input$symbol_size,
                              legend_position = input$legend_position
                              )
  })

  return(list(
    save = function() {
      all_treatments <- unique(common$treatment_df$Label)
      treatments <- all_treatments[all_treatments != common$reference_treatment_all]
      list(
      ### Manual save start
      treatments = treatments,
      comparators = input$comparators,
      ### Manual save end
      covariate = input$covariate,
      credible = input$credible,
      credible_opacity = input$credible_opacity,
      ghosts = input$ghosts,
      extrapolate = input$extrapolate,
      symbol = input$symbol,
      symbol_size = input$symbol_size,
      legend_position_dropdown = input$legend_position_dropdown)
    },
    load = function(state) {
      ### Manual load start
      updatePickerInput(session, "comparators", selected = state$comparators, choices = state$treatments)
      ### Manual load end
      updateCheckboxInput(session, "covariate", value = state$covariate)
      updateCheckboxInput(session, "credible", value = state$credible)
      updateSliderInput(session, "credible_opacity", value = state$credible_opacity)
      updateCheckboxInput(session, "ghosts", value = state$ghosts)
      updateCheckboxInput(session, "extrapolate", value = state$extrapolate)
      updateSelectInput(session, "symbol", selected = state$symbol)
      updateNumericInput(session, "symbol_size", value = state$symbol_size)
      updateSelectInput(session, "legend_position", selected = state$legend_position)
    }
  ))

})
}


covariate_regression_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"), height = "800px")
}


covariate_regression_module_rmd <- function(common){ list(
  covariate_regression_knit = !is.null(common$meta$covariate_regression$used),
  covariate_regression_comparators = common$meta$covariate_regression$comparators,
  covariate_regression_covariate = common$meta$covariate_regression$covariate,
  covariate_regression_credible = common$meta$covariate_regression$credible,
  covariate_regression_credible_opacity = common$meta$covariate_regression$credible_opacity,
  covariate_regression_ghosts = common$meta$covariate_regression$ghosts,
  covariate_regression_extrapolate = common$meta$covariate_regression$extrapolate,
  covariate_regression_symbol = common$meta$covariate_regression$symbol,
  covariate_regression_symbol_size = common$meta$covariate_regression$symbol_size,
  covariate_regression_legend_position = common$meta$covariate_regression$legend_position)
}

