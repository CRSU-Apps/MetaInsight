metaregression_regression_module_ui <- function(id, parent_id){

  ns <- NS(id)
  tagList(
    tags$div(
      tags$label(
        `for` = ns("comparators"),
        "Treatments to include ",
        tags$span(
          style = "font-weight: normal; font-size: 0.9em;",
          actionLink(ns("add_all_comparators"), "Add all", style = "margin-left: 10px;"),
          " | ",
          actionLink(ns("remove_all_comparators"), "Remove all")
        )
      ),
      shinyWidgets::pickerInput(
        ns("comparators"),
        label = NULL,
        choices = c(),
        multiple = TRUE
      )
    ),
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
    div(class = glue("{parent_id}_div download_buttons"),
        downloadButton(ns("download"), "Download plot")
    )
  )
}


covariate_regression_module_ui <- function(id) {
  ns <- NS(id)
  metaregression_regression_module_ui(ns("covariate"), id)
}


metaregression_regression_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    module_id <- glue("{id}_regression")
    model <- glue("{id}_model")
    model_fit <- glue("{id}_model_fit")

    init(glue("{module_id}_plot"))

    hide_and_show(module_id, show = FALSE)

    observe({
      watch("setup_configure")
      req(common$reference_treatment_all)
      all_treatments <- unique(common$treatment_df$Label)
      treatments <- all_treatments[all_treatments != common$reference_treatment_all]
      shinyWidgets::updatePickerInput(session, "comparators", choices = treatments, selected = character(0))
    })

    observeEvent(input$add_all_comparators, {
      req(common$reference_treatment_all)
      all_treatments <- unique(common$treatment_df$Label)
      treatments <- all_treatments[all_treatments != common$reference_treatment_all]
      shinyWidgets::updatePickerInput(session, "comparators", selected = treatments)
    })

    observeEvent(input$remove_all_comparators, {
      shinyWidgets::updatePickerInput(session, "comparators", selected = character(0))
    })

    observe({
      watch("setup_reset")
      shinyWidgets::updatePickerInput(session, "comparators", choices = c(), selected = character(0))
    })

    common$tasks[[module_id]] <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = get(module_id), .args = environment())
    ) |> bind_task_button("run")

    observeEvent(input$run, {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", go_to = glue("{id}_model"), glue("Please fit the {id} model first"))
        return()
      } else {
        trigger(module_id)
      }
    })

    observeEvent(list(watch(model_fit), watch(module_id)), {
      # trigger if run is pressed or if model is changed, but only if a model exists
      req((watch(module_id) > 0 && any(!is.null(common[[model]]), watch(model_fit) > 0)))

      if (is.null(common[[module_id]])){
        common$logger |> writeLog(type = "starting", glue("Calculating {id} regression plot data"))
      } else {
        common$logger |> writeLog(type = "starting", glue("Updating {id} regression plot data"))
      }

      common$tasks[[module_id]]$invoke(common[[model]],
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

      result <- common$tasks[[module_id]]$result()
      regress_result$suspend()
      if (inherits(result, "list")){
        common[[module_id]] <- result
        shinyjs::runjs(glue("Shiny.setInputValue('{module_id}-complete', 'complete');"))
        common$logger |> writeLog(type = "complete", glue("{stringr::str_to_title(id)} regression plot data has been calculated"))
      } else {
        common$logger |> writeLog(type = "error", result)
      }

      trigger(glue("{module_id}_plot"))
    })

    svg <- reactive({
      watch(module_id) # enable reset
      watch(glue("{module_id}_plot"))
      req(common[[module_id]])
      on.exit(shinyjs::show(selector = glue(".{module_id}_div")))

      # METADATA ####
      common$meta[[module_id]]$used <- TRUE
      common$meta[[module_id]]$comparators <- input$comparators
      common$meta[[module_id]]$covariate <- input$covariate
      common$meta[[module_id]]$credible <- input$credible
      common$meta[[module_id]]$credible_opacity <- as.numeric(input$credible_opacity)
      common$meta[[module_id]]$ghosts <- input$ghosts
      common$meta[[module_id]]$extrapolate <- input$extrapolate
      common$meta[[module_id]]$symbol <- input$symbol
      common$meta[[module_id]]$symbol_size <- as.numeric(input$symbol_size)
      common$meta[[module_id]]$legend_position <- input$legend_position

      metaregression_plot(model_output = common[[model]],
                          treatment_df = common$treatment_df,
                          outcome_measure = common$outcome_measure,
                          comparators = input$comparators,
                          directness = common[[module_id]]$directness,
                          credible_regions = common[[module_id]]$credible_regions,
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
        svg()
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        return(glue("MetaInsight_{id}_regression_plot.{common$download_format}"))
      },
      content = function(file) {
        write_plot(svg(), file, common$download_format)
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
  module_id <- strsplit(id, "-")[[1]][1]

  div(align = "center",
      uiOutput(ns("plot")),
      h5(class = glue("{module_id}_div"), style = "text-align:left",
         "This graph was adapted from",
         tags$em("Graphs of study contributions and covariate distributions for network meta-regression"),
         ", Sarah Donegan, Sofia Dias, Catrin Tudur-Smith, Valeria Marinho, Nicky J Welton, ",
         tags$em("Res Syn Meth"),
         ", 2018;",
         tags$b(9),
         ":243-260.",
         tags$b(tags$a("DOI: 10.1002/jrsm.1292", href = "https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1292", target = "_blank"))
         )
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

