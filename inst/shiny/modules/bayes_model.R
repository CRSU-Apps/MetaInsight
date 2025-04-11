bayes_model_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run_all"), "Run the main analysis for all studies"),
    actionButton(ns("run_sub"), "Run the analysis excluding selected studies")

  )
}

bayes_model_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(list(input$run_all, input$run_sub), {
    req(common$outcome_measure)
    if (common$outcome_measure == "SMD") {
      common$logger %>% writeLog(type = "error", "Standardised mean difference currently cannot be analysed within Bayesian analysis in MetaInsight")
    }
    else if (common$outcome_measure == "RD") {
      common$logger %>% writeLog(type = "error", "Risk difference currently cannot be analysed within Bayesian analysis in MetaInsight")
    }
  })

  observeEvent(input$run_all, {
    # WARNING ####

    # FUNCTION CALL ####

    common$bayes_all <- bayes_model(common$main_connected_data,
                                    common$treatment_df,
                                    common$outcome,
                                    common$outcome_measure,
                                    common$model_type,
                                    common$reference_treatment_all,
                                    common$logger)

    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$bayes_model$used <- TRUE

  })

  observeEvent(input$run_sub, {
    # WARNING ####

    # FUNCTION CALL ####

    common$bayes_sub <- bayes_model(common$subsetted_data,
                                    common$subsetted_treatment_df,
                                    common$outcome,
                                    common$outcome_measure,
                                    common$model_type,
                                    common$reference_treatment_sub,
                                    common$logger)

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
  })




})
}


