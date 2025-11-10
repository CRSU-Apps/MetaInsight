metaregression_regression_module_ui <- function(id, module){

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
    # make labels and input boxes appear inline rather than above and below
    div(class = "inline_labels",
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
      )
    ),
    input_task_button(ns("run"), "Generate plot", type = "default", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download plot")
  )
}


covariate_regression_module_ui <- function(id) {
  ns <- NS(id)
  metaregression_regression_module_ui(ns("covariate"), "covariate_regression")
}


metaregression_regression_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    module <- glue::glue("{id}_regression")
    model <- glue::glue("{id}_model")
    model_fit <- glue::glue("{id}_model_fit")

    shinyjs::hide("download")
    init(glue::glue("{module}_plot"))

    observe({
      watch("setup_configure")
      req(common$reference_treatment_all)
      all_treatments <- unique(common$treatment_df$Label)
      treatments <- all_treatments[all_treatments != common$reference_treatment_all]
      shinyWidgets::updatePickerInput(session, "comparators", choices = treatments, selected = treatments)
    })

    common$tasks[[module]] <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = get(module), .args = environment())
    ) |> bind_task_button("run")

    observeEvent(input$run, {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", glue::glue("Please fit the {id} model first"))
        return()
      } else {
        trigger(module)
      }
    })

    observeEvent(list(watch(model_fit), watch(module)), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch(module) > 0 && any(!is.null(common[[model]]), watch(model_fit) > 0)))

      if (is.null(common[[module]])){
        common$logger |> writeLog(type = "starting", glue::glue("Calculating {id} regression plot data"))
      } else {
        common$logger |> writeLog(type = "starting", glue::glue("Updating {id} regression plot data"))
      }

      common$tasks[[module]]$invoke(common[[model]],
                                     common$main_connected_data,
                                     ifelse(id == "covariate", common$covariate_column, "covar.baseline_risk"),
                                     common$treatment_df,
                                     common$outcome,
                                     common$outcome_measure,
                                     common$model_type,
                                     async = TRUE)

      regress_result$resume()
    })


    regress_result <- observe({

      result <- common$tasks[[module]]$result()
      regress_result$suspend()
      if (inherits(result, "list")){
        common[[module]] <- result
        shinyjs::runjs(glue::glue("Shiny.setInputValue('{module}-complete', 'complete');"))
        common$logger |> writeLog(type = "complete", glue::glue("{stringr::str_to_title(id)} regression plot data has been calculated"))
      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger(glue::glue("{module}_plot"))
    })

    svg <- reactive({
      watch(glue::glue("{module}_plot"))
      req(common[[module]])
      on.exit(shinyjs::show("download"))

      # METADATA ####
      common$meta[[module]]$used <- TRUE
      common$meta[[module]]$comparators <- input$comparators
      common$meta[[module]]$covariate <- input$covariate
      common$meta[[module]]$credible <- input$credible
      common$meta[[module]]$credible_opacity <- as.numeric(input$credible_opacity)
      common$meta[[module]]$ghosts <- input$ghosts
      common$meta[[module]]$extrapolate <- input$extrapolate
      common$meta[[module]]$symbol <- input$symbol
      common$meta[[module]]$symbol_size <- as.numeric(input$symbol_size)
      common$meta[[module]]$legend_position <- input$legend_position

      metaregression_plot(model_output = common[[model]],
                          treatment_df = common$treatment_df,
                          outcome_measure = common$outcome_measure,
                          comparators = input$comparators,
                          directness = common[[module]]$directness,
                          credible_regions = common[[module]]$credible_regions,
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

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container", style = "max-width: 1200px;",
        HTML(svg()$svg)
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        return(glue::glue("MetaInsight_{id}_regression_plot.{common$download_format}"))
      },
      content = function(file) {
        write_svg_plot(file, common$download_format, svg())
      }
    )

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
        shinyWidgets::updatePickerInput(session, "comparators", selected = state$comparators, choices = state$treatments)
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

covariate_regression_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_regression_module_server("covariate", common)
})
}

metaregression_regression_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      uiOutput(ns("plot"))
  )
}

covariate_regression_module_result <- function(id) {
  ns <- NS(id)
  metaregression_regression_module_result(ns("covariate"))
}

metaregression_regression_module_rmd <- function(common, module){

  template <- list(
    knit = !is.null(common$meta[[module]]$used),
    comparators = common$meta[[module]]$comparators,
    covariate = common$meta[[module]]$covariate,
    credible = common$meta[[module]]$credible,
    credible_opacity = common$meta[[module]]$credible_opacity,
    ghosts = common$meta[[module]]$ghosts,
    extrapolate = common$meta[[module]]$extrapolate,
    symbol = common$meta[[module]]$symbol,
    symbol_size = common$meta[[module]]$symbol_size,
    legend_position = common$meta[[module]]$legend_position
  )

  setNames(template, paste(module, names(template), sep = "_"))

  }

covariate_regression_module_rmd <- function(common){
  metaregression_regression_module_rmd(common, "covariate_regression")
}

