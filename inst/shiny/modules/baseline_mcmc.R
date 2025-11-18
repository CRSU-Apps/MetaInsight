baseline_mcmc_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down"))
  )
}

baseline_mcmc_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id, show = FALSE)

    common$tasks$baseline_mcmc <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = baseline_mcmc, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(list(input$run, watch("baseline_model_fit")), {
      req(watch("baseline_mcmc") > 0 || input$run > 0)

      if (is.null(common$baseline_model)){
        common$logger |> writeLog(type = "error", go_to = "baseline_model", "Please fit the baseline model first")
        return()
      }
      if (is.null(common$baseline_mcmc)){
        common$logger |> writeLog(type = "starting", "Generating data for baseline risk Markov chain Monte Carlo plots")
      } else {
        common$logger |> writeLog(type = "starting", "Updating data for baseline risk Markov chain Monte Carlo plots")
      }
      common$tasks$baseline_mcmc$invoke(common$baseline_model)
      common$meta$baseline_mcmc$used <- TRUE
      result_all$resume()
    })

    result_all <- observe({
      result <- common$tasks$baseline_mcmc$result()
      result_all$suspend()
      if (is.null(common$baseline_mcmc)){
        common$logger |> writeLog(type = "complete", "Data for baseline risk Markov chain Monte Carlo plots has been generated")
      } else {
        common$logger |> writeLog(type = "complete", "Data for baseline risk Markov chain Monte Carlo plots has been updated")
      }
      common$baseline_mcmc <- result
      trigger("baseline_mcmc")
    })

    bayes_mcmc_submodule_server("all", common, "baseline_mcmc", "baseline_model", "baseline_mcmc", "baseline_mcmc")

  })
}

baseline_mcmc_module_result <- function(id) {
  ns <- NS(id)
  bayes_mcmc_submodule_result(ns("all"), "baseline_mcmc_div", "for all studies")
}

baseline_mcmc_module_rmd <- function(common) {
  list(baseline_mcmc_knit = !is.null(common$meta$baseline_mcmc$used),
       baseline_mcmc_n_rows = common$meta$baseline_mcmc$n_rows,
       baseline_mcmc_height_200 = common$meta$baseline_mcmc$n_rows * (200/72),
       baseline_mcmc_height_250 = common$meta$baseline_mcmc$n_rows * (250/72))
}
