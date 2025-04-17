bayes_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # bslib::input_task_button(ns("run_all"), "Run the main analysis for all studies"),
    # bslib::input_task_button(ns("run_sub"), "Run the analysis excluding selected studies")
    bslib::input_task_button(ns("run"), "Run models")
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
      function(fun, ...) mirai::mirai(fun(...), environment())
    ) %>% bslib::bind_task_button("run")

    # sub_model <- NULL
    # common$tasks$bayes_model_sub <- ExtendedTask$new(
    #   function(fun, ...) sub_model <<- mirai::mirai(fun(...), environment())
    # ) %>% bslib::bind_task_button("run")

    common$tasks$bayes_model_sub <- ExtendedTask$new(
      function(fun, ...) mirai::mirai(fun(...), environment())
    ) %>% bslib::bind_task_button("run")

    observeEvent(watch("bayes_model"), {
      req(watch("bayes_model") > 0)
      common$tasks$bayes_model_all$invoke(bayes_model,
                                          common$main_connected_data,
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

      # if (common$tasks$bayes_model_sub$status() == "running"){
      #   print("cancelling")
      #   mirai::stop_mirai(sub_model)
      # }

      # FUNCTION CALL ####
      common$tasks$bayes_model_sub$invoke(bayes_model,
                                          common$subsetted_data,
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
      common$bayes_sub <- common$tasks$bayes_model_sub$result()
      result_sub$suspend()
      trigger("bayes_model_sub")
    })


  # observeEvent(list(input$run_all, input$run_sub), {
  #   req(common$outcome_measure)
  #   if (common$outcome_measure == "SMD") {
  #     common$logger %>% writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
  #   }
  #   else if (common$outcome_measure == "RD") {
  #     common$logger %>% writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
  #   }
  # })
  #
  # common$tasks$bayes_model_all <- ExtendedTask$new(
  #   function(run, ...) mirai::mirai(run(...), environment())
  # ) %>% bslib::bind_task_button("run_all")
  #
  # common$tasks$bayes_model_sub <- ExtendedTask$new(
  #   function(run, ...) mirai::mirai(run(...), environment())
  # ) %>% bslib::bind_task_button("run_sub")
  #
  #
  # observeEvent(input$run_all, {
  #
  #   # FUNCTION CALL ####
  #   common$tasks$bayes_model_all$invoke(bayes_model,
  #                                       common$main_connected_data,
  #                                       common$treatment_df,
  #                                       common$outcome,
  #                                       common$outcome_measure,
  #                                       common$model_type,
  #                                       common$reference_treatment_all)
  #
  #   # METADATA ####
  #   common$meta$bayes_model$used <- TRUE
  #   result_all$resume()
  #
  # })
  #
  # observeEvent(list(input$run_sub, watch("summary_exclude")), {
  #
  #   req(input$run_sub > 0)
  #
  #   # FUNCTION CALL ####
  #   common$tasks$bayes_model_sub$invoke(bayes_model,
  #                                       common$subsetted_data,
  #                                       common$subsetted_treatment_df,
  #                                       common$outcome,
  #                                       common$outcome_measure,
  #                                       common$model_type,
  #                                       common$reference_treatment_sub)
  #
  #   result_sub$resume()
  # })
  #
  #
  # result_all <- observe({
  #   common$bayes_all <- common$tasks$bayes_model_all$result()
  #   result_all$suspend()
  # })
  #
  # result_sub <- observe({
  #   common$bayes_sub <- common$tasks$bayes_model_sub$result()
  #   result_sub$suspend()
  # })
})
}


