covariate_deviance_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

covariate_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".covariate_deviance_div")

    common$tasks$covariate_deviance <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = covariate_deviance, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(input$run, watch("covariate_model_fit")), {
      req(watch("covariate_deviance") > 0 || input$run > 0)

      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "error", "Please fit the covariate model first")
        return()
      }

      common$logger |> writeLog(type = "starting", "Generating covariate deviance plots")
      common$tasks$covariate_deviance$invoke(common$covariate_model)
      common$meta$covariate_deviance$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks$covariate_deviance$result()
      result_all$suspend()
      common$covariate_deviance <- result
      common$logger |> writeLog(type = "complete", "Covariate deviance plots have been generated")
      trigger("covariate_deviance")
      shinyjs::show(selector = ".covariate_deviance_div")
    })

    output$scat <- plotly::renderPlotly({
      watch("covariate_deviance")
      req(common$covariate_deviance)
      common$covariate_deviance$scat_plot
    })

    output$stem <- plotly::renderPlotly({
      watch("covariate_deviance")
      req(common$covariate_deviance)
      common$covariate_deviance$stem_plot
    })

    output$lev <- plotly::renderPlotly({
      watch("covariate_deviance")
      req(common$covariate_deviance)
      common$covariate_deviance$lev_plot
    })

})
}

covariate_deviance_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(align = "center",
      div(class = "covariate_deviance_div", style = "max-width: 800px;",
        h5("Residual deviance from NMA model and UME inconsistency model for covariate model"),
        plotly::plotlyOutput(ns("scat")),
        h5("Per-arm residual deviance for covariate model"),
        plotly::plotlyOutput(ns("stem")),
        h5("Leverage plot for covariate model"),
        plotly::plotlyOutput(ns("lev"))
      )
    )
  )
}

covariate_deviance_module_rmd <- function(common) {
  list(covariate_deviance_knit = !is.null(common$meta$covariate_deviance$used))
}

