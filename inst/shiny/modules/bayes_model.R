bayes_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Run models", type = "default")
  )
}

bayes_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_model_sub")

    observeEvent(input$run, {
      req(common$outcome_measure)
      if (common$outcome_measure == "SMD") {
        common$logger %>% writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
      }
      else if (common$outcome_measure == "RD") {
        common$logger %>% writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
      }
      trigger("bayes_model")
    })

    common$tasks$bayes_model_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_model, .args = environment())
    ) %>% bind_task_button("run")

    # needed to cancel in progress
    sub_model <- NULL
    common$tasks$bayes_model_sub <- ExtendedTask$new(
      function(...) sub_model <<- mirai::mirai(run(...), run = bayes_model, .args = environment())
    ) %>% bind_task_button("run")

    observeEvent(watch("bayes_model"), {
      req(watch("bayes_model") > 0)
      common$tasks$bayes_model_all$invoke(common$main_connected_data,
                                          common$treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_all)

      # METADATA ####
      common$meta$bayes_model$used <- TRUE
      result_all$resume()
    })

    observeEvent(list(watch("bayes_model"), watch("summary_exclude")), {
      req(watch("bayes_model") > 0)

      # cancel if the model is already updating
      if (common$tasks$bayes_model_sub$status() == "running"){
        mirai::stop_mirai(sub_model)
      }

      shinyjs::show(selector = ".bayes_sub")

      common$tasks$bayes_model_sub$invoke(common$subsetted_data,
                                          common$subsetted_treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_sub)
      result_sub$resume()
    })


    result_all <- observe({
      common$bayes_all <- common$tasks$bayes_model_all$result()
      result_all$suspend()
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$bayes_model_sub$status() == "success"){
        common$bayes_sub <- common$tasks$bayes_model_sub$result()
        result_sub$suspend()
        trigger("bayes_model_sub")
      }
    })

})
}


