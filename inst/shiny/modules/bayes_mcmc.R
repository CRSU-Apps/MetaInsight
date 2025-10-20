bayes_mcmc_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

bayes_mcmc_submodule_server <- function(id, common, module_id, model, mcmc, trigger){
  moduleServer(id, function(input, output, session) {

    # n_cols doesn't need storing as always = 2 in the Rmd
    n_rows <- reactive({
      watch(trigger)
      req(common[[mcmc]])
      common$meta[[module_id]]$n_rows <- common[[mcmc]]$n_rows_rmd
      common[[mcmc]]$n_rows
    })

    output$gelman <- renderPlot({
      watch(trigger)
      req(common[[mcmc]])
      cowplot::plot_grid(
        plotlist = gelman_plots(common[[mcmc]]$gelman_data, common[[mcmc]]$parameters),
        ncol = common[[mcmc]]$n_cols
      )
    }, height = function() {
      n_rows() * 250
    })

    output$trace <- renderPlot({
      watch(trigger)
      req(common[[mcmc]])
      cowplot::plot_grid(
        plotlist = trace_plots(common[[model]]$mtcResults, common[[mcmc]]$parameters),
        ncol = common[[mcmc]]$n_cols
      )
    }, height = function() {
      n_rows() * 200
    })

    output$density <- renderPlot({
      watch(trigger)
      req(common[[mcmc]])
      on.exit(shinyjs::runjs(paste0("Shiny.setInputValue('", mcmc, "-complete', 'complete');")))
      cowplot::plot_grid(
        plotlist = density_plots(common[[model]]$mtcResults, common[[mcmc]]$parameters),
        ncol = common[[mcmc]]$n_cols
      )
    }, height = function() {
      n_rows() * 200
    })

  })
}


bayes_mcmc_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_mcmc_all")
    init("bayes_mcmc_sub")
    shinyjs::hide(selector = ".bayes_mcmc_div")

    observeEvent(input$run, {
      # add check for a running model

      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_mcmc$used <- TRUE
        trigger("bayes_mcmc")
      }
    })

    common$tasks$bayes_mcmc_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_mcmc, .args = environment())
    ) |> bind_task_button("run")

    common$tasks$bayes_mcmc_sub <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_mcmc, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(watch("bayes_mcmc"), watch("bayes_model_all")), {
      req(watch("bayes_mcmc") > 0)
      common$logger |> writeLog(type = "starting", "Generating data for Markov chain Monte Carlo plots")
      common$tasks$bayes_mcmc_all$invoke(common$bayes_all)
      result_all$resume()
    })

    observeEvent(list(watch("bayes_mcmc"), watch("bayes_model_sub")), {
      req(watch("bayes_mcmc") > 0)

      # prevent showing on first run
      if (!is.null(common$bayes_mcmc_sub)){
        common$logger |> writeLog(type = "starting", "Updating data for Markov chain Monte Carlo plots")
      }

      common$tasks$bayes_mcmc_sub$invoke(common$bayes_sub)
      result_sub$resume()
    })

    result_all <- observe({
      result <- common$tasks$bayes_mcmc_all$result()
      result_all$suspend()
      common$bayes_mcmc_all <- result
      common$logger |> writeLog(type = "complete", "Data for Markov chain Monte Carlo plots has been generated")
      shinyjs::show(selector = ".bayes_mcmc_div")
      trigger("bayes_mcmc_all")
    })

    result_sub <- observe({
      result <- common$tasks$bayes_mcmc_sub$result()
      result_sub$suspend()
      if (!is.null(common$bayes_mcmc_sub)){
        common$logger |> writeLog(type = "complete", "Data for Markov chain Monte Carlo plots has been updated")
      }
      common$bayes_mcmc_sub <- result
      trigger("bayes_mcmc_sub")
    })

    bayes_mcmc_submodule_server("all", common, "bayes_mcmc", "bayes_model_all", "bayes_mcmc_all", "bayes_mcmc_all")
    bayes_mcmc_submodule_server("sub", common, "bayes_mcmc", "bayes_model_sub", "bayes_mcmc_sub", "bayes_mcmc_sub")

  })
}

bayes_mcmc_submodule_result <- function(id, class, label) {
  ns <- NS(id)
  tagList(
    div(class = class,
      h5(glue::glue("Gelman convergence assessment plots {label}")),
      # auto makes the output height the same as the render
      plotOutput(ns("gelman"), height = "auto"),
      h5(glue::glue("Trace plots {label}")),
      plotOutput(ns("trace"), height = "auto"),
      h5(glue::glue("Posterior density plots {label}")),
      plotOutput(ns("density"), height = "auto")
    )
  )
}

bayes_mcmc_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        align = "center",
        bayes_mcmc_submodule_result(ns("all"), "bayes_mcmc_div", "for all studies")
      ),
      column(
        width = 6,
        align = "center",
        bayes_mcmc_submodule_result(ns("sub"), "bayes_mcmc_div", "excluding selected studies")
      )
    )
  )
}

bayes_mcmc_module_rmd <- function(common) {
  list(bayes_mcmc_knit = !is.null(common$meta$bayes_mcmc$used),
       bayes_mcmc_n_rows = common$meta$bayes_mcmc$n_rows,
       bayes_mcmc_height_200 = common$meta$bayes_mcmc$n_rows * (200/72),
       bayes_mcmc_height_250 = common$meta$bayes_mcmc$n_rows * (250/72))
}
