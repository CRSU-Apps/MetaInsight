covariate_mcmc_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

covariate_mcmc_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".covariate_mcmc_div")

    common$tasks$covariate_mcmc <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = covariate_mcmc, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(input$run, watch("covariate_model_fit")), {
      req(watch("covariate_mcmc") > 0 || input$run > 0)

      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "error", "Please fit the covariate model first")
        return()
      }

      common$logger |> writeLog(type = "starting", "Generating covariate Markov chain Monte Carlo plots")
      common$tasks$covariate_mcmc$invoke(common$covariate_model)
      common$meta$covariate_mcmc$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks$covariate_mcmc$result()
      result_all$suspend()
      common$covariate_mcmc <- result
      common$logger |> writeLog(type = "complete", "Covariate Markov chain Monte Carlo plots have been generated")
      trigger("covariate_mcmc")
      shinyjs::show(selector = ".covariate_mcmc_div")
    })

    n_rows <- reactive({
      watch("covariate_mcmc")
      req(common$covariate_mcmc)
      n <- common$covariate_mcmc$n_rows
      common$meta$covariate_mcmc$n_rows <- n
      n
    })

    output$gelman <- renderPlot({
      watch("covariate_mcmc")
      req(common$covariate_mcmc)
      shinyjs::show(selector = ".covariate_mcmc_div")
      par(mfrow = c(2, n_rows()))
      # this returns a list of functions, each of which generates a plot
      invisible(lapply(common$covariate_mcmc$gelman_plots, function(f) f()))
    }, height = function() {
      n_rows() * 250
    })

    output$trace <- renderPlot({
      watch("covariate_mcmc")
      req(common$covariate_mcmc)
      cowplot::plot_grid(
        plotlist = common$covariate_mcmc$trace_plots,
        ncol = 2
      )
    }, height = function() {
      n_rows() * 200
    })

    output$density <- renderPlot({
      watch("covariate_mcmc")
      req(common$covariate_mcmc)
      on.exit(shinyjs::runjs("Shiny.setInputValue('covariate_mcmc-complete', 'complete');"))
      cowplot::plot_grid(
        plotlist = common$covariate_mcmc$density_plots,
        ncol = 2
      )
    }, height = function() {
      n_rows() * 200
    })


})
}


covariate_mcmc_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(align = "center",
      div(class = "covariate_mcmc_div", style = "max-width: 800px;",
        h5("Gelman convergence assessment plots for covariate model"),
        # auto makes the output height the same as the render
        plotOutput(ns("gelman"), height = "auto"),
        h5("Trace plots for covariate model"),
        plotOutput(ns("trace"), height = "auto"),
        h5("Posterior density plots for covariate model"),
        plotOutput(ns("density"), height = "auto")
      )
    )
  )
}


covariate_mcmc_module_rmd <- function(common) {
  list(covariate_mcmc_knit = !is.null(common$meta$covariate_mcmc$used),
       covariate_mcmc_n_rows = common$meta$covariate_mcmc$n_rows,
       covariate_mcmc_height_200 = common$meta$covariate_mcmc$n_rows * (200/72),
       covariate_mcmc_height_250 = common$meta$covariate_mcmc$n_rows * (250/72))
}

