covariate_mcmc_module_ui <- function(id) {
  ns <- NS(id)
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
      if (is.null(common$covariate_mcmc)){
        common$logger |> writeLog(type = "starting", "Generating data for covariate Markov chain Monte Carlo plots")
      } else {
        common$logger |> writeLog(type = "starting", "Updating data for covariate Markov chain Monte Carlo plots")
      }
      common$tasks$covariate_mcmc$invoke(common$covariate_model)
      common$meta$covariate_mcmc$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks$covariate_mcmc$result()
      result_all$suspend()
      if (is.null(common$covariate_mcmc)){
        common$logger |> writeLog(type = "starting", "Data for covariate Markov chain Monte Carlo plots has been generated")
      } else {
        common$logger |> writeLog(type = "starting", "Data for covariate Markov chain Monte Carlo plots has been updated")
      }
      common$covariate_mcmc <- result
      trigger("covariate_mcmc")
    })

    bayes_mcmc_submodule_server("all", common, "covariate_mcmc", "covariate_model", "covariate_mcmc", "covariate_mcmc")

})
}

covariate_mcmc_module_result <- function(id) {
  ns <- NS(id)
  bayes_mcmc_submodule_result(ns("all"), "covariate_mcmc_div", "for all studies")
}

covariate_mcmc_module_rmd <- function(common) {
  list(covariate_mcmc_knit = !is.null(common$meta$covariate_mcmc$used),
       covariate_mcmc_n_rows = common$meta$covariate_mcmc$n_rows,
       covariate_mcmc_height_200 = common$meta$covariate_mcmc$n_rows * (200/72),
       covariate_mcmc_height_250 = common$meta$covariate_mcmc$n_rows * (250/72))
}

