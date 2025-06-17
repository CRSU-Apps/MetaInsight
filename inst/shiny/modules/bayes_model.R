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
      if (is.null(common$main_connected_data)){
        common$logger %>% writeLog(type = "error", "Please define the data in the Setup component first.")
      }
      if (common$outcome_measure == "SMD") {
        common$logger %>% writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
      }
      else if (common$outcome_measure == "RD") {
        common$logger %>% writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
      }
      # METADATA ####
      common$meta$bayes_model$used <- TRUE
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
      common$logger %>% writeLog(type = "starting", "Fitting Bayesian models")
      common$tasks$bayes_model_all$invoke(common$main_connected_data,
                                          common$treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_all,
                                          async = TRUE)


      result_all$resume()
    })

    observeEvent(list(watch("bayes_model"), watch("summary_exclude")), {
      req((watch("bayes_model") + watch("summary_exclude")) > 0)
      req(common$meta$bayes_model$used)

      # cancel if the model is already updating
      if (common$tasks$bayes_model_sub$status() == "running"){
        mirai::stop_mirai(sub_model)
      }

      # prevent showing on first run
      if (!is.null(common$bayes_sub)){
        common$logger %>% writeLog(type = "starting", "Updating Bayesian for sensitivity analysis")
      }

      common$tasks$bayes_model_sub$invoke(common$subsetted_data,
                                          common$subsetted_treatment_df,
                                          common$outcome,
                                          common$outcome_measure,
                                          common$model_type,
                                          common$reference_treatment_sub,
                                          async = TRUE)
      result_sub$resume()
    })


    result_all <- observe({

      result <- common$tasks$bayes_model_all$result()
      result_all$suspend()
      if (inherits(result, "list")){
        common$bayes_all <- result
        common$logger %>% writeLog(type = "complete", "Bayesian models have been fitted")
      } else {
        common$logger %>% writeLog(type = "error", result)
      }
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$bayes_model_sub$status() == "success"){
        result <- common$tasks$bayes_model_sub$result()
        result_sub$suspend()
        if (inherits(result, "list")){
          # prevent showing on first run
          if (!is.null(common$bayes_sub)){
            common$logger %>% writeLog(type = "complete", "The Bayesian model for the sensitivity analysis has been updated")
          }
          common$bayes_sub <- result
        } else {
          common$logger %>% writeLog(type = "error", result)
        }
        trigger("bayes_model_sub")
      }
    })

})
}


bayes_model_module_rmd <- function(common) {
  list(bayes_model_knit = !is.null(common$bayes_all))
}

