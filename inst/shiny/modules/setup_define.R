setup_define_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectizeInput(ns("reference_treatment"), "Select reference treatment", choices = c()),
    uiOutput(ns("outcome_out")),
    radioButtons(
      inputId = ns("rankopts"),
      label = 'For treatment rankings, smaller outcome values (e.g. smaller mean values for continuous data, or ORs less than 1 for binary data) are:',
      choices = c(
        "Desirable" = "good",
        "Undesirable" = "bad"
      )
    ),
    actionButton(ns("run"), "Define data")
  )
}

setup_define_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  output$outcome_out <- renderUI({
    gargoyle::watch("setup_load")
    req(common$metaoutcome)
    if (common$metaoutcome == "Continuous"){
      radioButtons(session$ns("outcome"),
                   label = "Outcome for continuous data:",
                   choices = c(
                     "Mean Difference (MD)" = "MD",
                     "Standardised Mean Difference (SMD)" = "SMD"
                   ))
    } else {
      radioButtons(session$ns("outcome"),
                   label = "Outcome for binary data:",
                   choices = c(
                     "Odds Ratio (OR)" = "OR",
                     "Risk Ratio (RR)" = "RR",
                     "Risk Difference (RD)" = "RD"
                   ))
    }
  })

  observe({
    gargoyle::watch("setup_load")
    req(common$treatment_df)
    treatments <- common$treatment_df$Label
    updateSelectInput(session, "reference_treatment", choices = treatments,
                      selected = FindExpectedReferenceTreatment(treatments))
    updateRadioButtons(session, "rankopts", selected = RankingOrder(common$metaoutcome, !common$is_data_uploaded))
  })

  output$metaoutcome <- reactive({
    gargoyle::watch("setup_load")
    common$metaoutcome
  })
  shiny::outputOptions(output, "metaoutcome", suspendWhenHidden = FALSE)

  observeEvent(input$run, {
    # WARNING ####

    # need data to exist

    # FUNCTION CALL ####
    result <- setup_define(common$data,
                          common$treatment_df,
                          common$metaoutcome,
                          input$reference_treatment,
                          common$logger)

    # LOAD INTO COMMON ####
    common$disconnected_indices <- result$disconnected_indices
    common$main_connected_data <- result$main_connected_data
    common$initial_non_covariate_data <- result$initial_non_covariate_data
    common$bugsnetdt <- result$bugsnetdt
    common$reference_treatment <- input$reference_treatment
    common$treatment_df <- result$treatment_df
    common$outcome_measure <- input$outcome
    common$logger %>% writeLog(type = "complete", "Data has been defined")

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("setup_define")
    show_table(parent_session)
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}


setup_define_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

