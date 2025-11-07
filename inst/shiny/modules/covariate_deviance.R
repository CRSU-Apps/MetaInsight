covariate_deviance_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

metaregression_deviance_module_server <- function(id, common, run) {
  moduleServer(id, function(input, output, session) {

    module <- glue::glue("{id}_deviance")
    model <- glue::glue("{id}_model")
    model_fit <- glue::glue("{id}_model_fit")
    shinyjs::hide(selector = glue::glue(".{module}_div"))

    common$tasks[[module]] <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = get(module), .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(run(), watch(model_fit)), {
      req(watch(module) > 0 || run() > 0)

      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", glue::glue("Please fit the {id} model first"))
        return()
      }

      common$logger |> writeLog(type = "starting", glue::glue("Generating {id} deviance plots"))
      common$tasks[[module]]$invoke(common[[model]])
      common$meta[[module]]$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks[[module]]$result()
      result_all$suspend()
      common[[module]] <- result
      common$logger |> writeLog(type = "complete", glue::glue("Covariate {id} plots have been generated"))
      trigger(module)
      shinyjs::show(selector = glue::glue(".{module}_div"))
    })

    output$stem <- plotly::renderPlotly({
      watch(module)
      req(common[[module]])
      common[[module]]$stem_plot
    })

    outputOptions(output, "stem", suspendWhenHidden = FALSE)

    output$lev <- plotly::renderPlotly({
      watch(module)
      req(common[[module]])
      common[[module]]$lev_plot
    })

  })
}

covariate_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_deviance_module_server("covariate", common, reactive(input$run))
})
}

metaregression_deviance_module_result <- function(id, package, class) {
  ns <- NS(id)
  tagList(
    div(align = "center",
        div(class = class, style = "max-width: 800px;",
            h4(glue::glue("The {package} package does not currently include unrelated-mean-effects meta-regression models, therefore the consistency vs UME graph that is displayed in the deviance report module of the Bayesian network meta-analysis section is not available here.")),
            h5("Per-arm residual deviance for covariate model"),
            plotly::plotlyOutput(ns("stem")),
            deviance_annotations[[2]],
            h5("Leverage plot for covariate model"),
            plotly::plotlyOutput(ns("lev")),
            deviance_annotations[[3]]
        )
    )
  )
}

covariate_deviance_module_result <- function(id) {
  ns <- NS(id)
  metaregression_deviance_module_result(ns("covariate"), "gemtc", "covariate_deviance_div")
}

covariate_deviance_module_rmd <- function(common) {
  list(covariate_deviance_knit = !is.null(common$meta$covariate_deviance$used))
}

