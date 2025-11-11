covariate_deviance_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

metaregression_deviance_module_server <- function(id, common, run) {
  moduleServer(id, function(input, output, session) {

    module_id <- glue::glue("{id}_deviance")
    model <- glue::glue("{id}_model")
    model_fit <- glue::glue("{id}_model_fit")
    shinyjs::hide(selector = glue::glue(".{module_id}_div"))

    common$tasks[[module_id]] <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = get(module_id), .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(run(), watch(model_fit)), {
      req(watch(module_id) > 0 || run() > 0)

      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", go_to = model, glue::glue("Please fit the {id} model first"))
        return()
      }

      common$logger |> writeLog(type = "starting", glue::glue("Generating {id} deviance plots"))
      common$tasks[[module_id]]$invoke(common[[model]])
      common$meta[[module_id]]$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks[[module_id]]$result()
      result_all$suspend()
      common[[module_id]] <- result
      common$logger |> writeLog(type = "complete", glue::glue("{stringr::str_to_sentence(id)} deviance plots have been generated"))
      trigger(module_id)
    })

    output$stem <- plotly::renderPlotly({
      watch(module_id)
      req(common[[module_id]])
      on.exit(shinyjs::show(selector = glue::glue(".{module_id}_div")))
      # workaround for testing
      on.exit(shinyjs::runjs(glue::glue("Shiny.setInputValue('{module_id}-complete', 'complete');")), add = TRUE)
      common[[module_id]]$stem_plot
    })

    outputOptions(output, "stem", suspendWhenHidden = FALSE)

    output$lev <- plotly::renderPlotly({
      watch(module_id)
      req(common[[module_id]])
      common[[module_id]]$lev_plot
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

