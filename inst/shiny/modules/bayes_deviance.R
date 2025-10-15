deviance_annotations <- list(
  p(
    "This plot represents each data point's contribution to the residual deviance for the
        NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models
        (vertical axis) along with the line of equality. The points on the equality line means there is no
        improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency.
        Points above the equality line means they have a smaller residual deviance for the consistency model indicating a
        better fit in the NMA consistency model and points below the equality line
        means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model
        may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
        decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
  ),
  p(
    "This stem plot represents the posterior residual deviance per study arm. The total number of stems equals
      the total number of data points in the network meta analysis. Going from left to right, the alternating symbols
      on the stems indicate the different studies. Each stem corresponds to the residual deviance ($dev.ab) associated with each
      arm in each study. The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each
      data point. You can identify which stem corresponds to which study arm by hovering on the stem symbols.
      (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
      decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
  ),
  p(
    "This leverage plot shows the average leverage across the arms for each study ({sum($lev.ab)}/{number of arms}
      for each study) versus the square root of the average residual deviance across the arms for each study
      (sqrt({sum($dev.ab)}/{number of arms}) for each study).
      The leverage for each data point, is calculated as the posterior mean of the residual
      deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to
      identify influential and/or poorly fitting studies and can be used to check how each study is affecting
      the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root
      of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas
      each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with
      c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are
      influential, which means that they have a strong influence on the model parameters that generate their fitted
      values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
      decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.
      Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4,
      pp.583-639)"
  )
)

bayes_deviance_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

bayes_deviance_submodule_server <- function(id, common, trigger){
  moduleServer(id, function(input, output, session) {

    output$scat <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      on.exit(shinyjs::show(selector = ".bayes_deviance_div"))
      common[[paste0("bayes_deviance_", id)]]$scat_plot
    })

    outputOptions(output, "scat", suspendWhenHidden = FALSE)

    output$stem <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      common[[paste0("bayes_deviance_", id)]]$stem_plot
    })

    output$lev <- plotly::renderPlotly({
      watch(trigger)
      req(common[[paste0("bayes_deviance_", id)]])
      common[[paste0("bayes_deviance_", id)]]$lev_plot
    })

  })
}


bayes_deviance_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_deviance_all")
    init("bayes_deviance_sub")
    shinyjs::hide(selector = ".bayes_deviance_div")

    observeEvent(input$run, {
      # add check for a running model

      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_deviance$used <- TRUE
        trigger("bayes_deviance")
      }
    })

    common$tasks$bayes_deviance_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_deviance, .args = environment())
    ) |> bind_task_button("run")

    common$tasks$bayes_deviance_sub <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_deviance, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("bayes_deviance"), watch("bayes_model_all")), {
      req(watch("bayes_deviance") > 0)
      common$logger |> writeLog(type = "starting", "Generating Bayesian deviance plots")
      common$tasks$bayes_deviance_all$invoke(common$bayes_all)
      result_all$resume()
    })

    observeEvent(list(watch("bayes_deviance"), watch("bayes_model_sub")), {
      req(watch("bayes_deviance") > 0)

      # prevent showing on first run
      if (!is.null(common$bayes_deviance_sub)){
        common$logger |> writeLog(type = "starting", "Updating Bayesian deviance plots")
      }

      common$tasks$bayes_deviance_sub$invoke(common$bayes_sub)
      result_sub$resume()
    })

    result_all <- observe({
      result <- common$tasks$bayes_deviance_all$result()
      result_all$suspend()
      common$bayes_deviance_all <- result
      common$logger |> writeLog(type = "complete", "Bayesian deviance plots have been generated")
      trigger("bayes_deviance_all")
    })

    result_sub <- observe({
      result <- common$tasks$bayes_deviance_sub$result()
      result_sub$suspend()
        if (!is.null(common$bayes_deviance_sub)){
          common$logger |> writeLog(type = "complete", "Bayesian deviance plots have been updated")
        }
      common$bayes_deviance_sub <- result
      trigger("bayes_deviance_sub")
    })

    bayes_deviance_submodule_server("all", common, "bayes_deviance_all")
    bayes_deviance_submodule_server("sub", common, "bayes_deviance_sub")

})
}

bayes_deviance_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "bayes_deviance_div",
      # this is a bit unusual as we are using the non-namespaced ids to allow the plot | plot, annotation layout
      layout_columns(
        div(
          h5(glue::glue("Residual deviance from NMA model and UME inconsistency model for all studies")),
          plotly::plotlyOutput("bayes_deviance-all-scat")
        ),
        div(
          h5(glue::glue("Residual deviance from NMA model and UME inconsistency model excluding selected studies")),
          plotly::plotlyOutput("bayes_deviance-sub-scat")
        ),
      ),
      deviance_annotations[[1]],
      layout_columns(
        div(
          h5(glue::glue("Per-arm residual deviance for all studies")),
          plotly::plotlyOutput("bayes_deviance-all-stem")
        ),
        div(
          h5(glue::glue("Per-arm residual deviance excluding selected studies")),
          plotly::plotlyOutput("bayes_deviance-sub-stem")
        ),
      ),
      deviance_annotations[[2]],
      layout_columns(
        div(
          h5(glue::glue("Leverage plot for all studies")),
          plotly::plotlyOutput("bayes_deviance-all-lev")
        ),
        div(
          h5(glue::glue("Leverage plot excluding selected studies")),
          plotly::plotlyOutput("bayes_deviance-sub-lev")
        ),
      ),
      deviance_annotations[[3]]
    )
  )
}

bayes_deviance_module_rmd <- function(common) {
  list(bayes_deviance_knit = !is.null(common$meta$bayes_deviance$used))
}



