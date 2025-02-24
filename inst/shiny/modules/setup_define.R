setup_define_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectizeInput(ns("reference_treatment"), "Select reference treatment", choices = c()),
    radioButtons(ns("outcome_measure"), label = "Outcome for continuous data:",
                 choices = c("Mean Difference (MD)" = "MD", "Standardised Mean Difference (SMD)" = "SMD")),
    radioButtons(ns("rankopts"), label = "For treatment rankings, values lower than the mean are:",
                 choices = c("Desirable" = "good", "Undesirable" = "bad")),
    actionButton(ns("run"), "Define data")
  )
}

setup_define_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  # Update UI depending on selections in previous module
  observe({
    watch("setup_load")
    req(common$treatment_df)
    treatments <- common$treatment_df$Label
    if (common$outcome == "Continuous"){
      outcome_label <- "Outcome for continuous data:"
      outcome_choices <- c("Mean Difference (MD)" = "MD", "Standardised Mean Difference (SMD)" = "SMD")
      rank_label <- "For treatment rankings, values lower than the mean are:"
    } else {
      outcome_label <- "Outcome for binary data:"
      outcome_choices <- c("Odds Ratio (OR)" = "OR", "Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD")
      # I've just edited the previous text, but what about RR or RD (SS)?
      rank_label <- "For treatment rankings, ORs less than 1 are:"
    }
    updateSelectInput(session, "reference_treatment", choices = treatments,
                      selected = FindExpectedReferenceTreatment(treatments))
    updateRadioButtons(session, "outcome_measure", outcome_label, outcome_choices)
    updateRadioButtons(session, "rankopts", rank_label,
                       selected = RankingOrder(common$outcome, !common$is_data_uploaded))
  })

  observeEvent(input$run, {
    # WARNING ####

    if (is.null(common$data)) {
      common$logger %>% writeLog(type = "error", "Please load data first")
      return()
    }

    if (!common$is_data_valid) {
      common$logger %>% writeLog(type = "error", "Please upload valid data first")
      return()
    }

    show_loading_modal("Processing data...")

    # FUNCTION CALL ####
    result <- setup_define(common$data,
                          common$treatment_df,
                          common$outcome,
                          input$outcome_measure,
                          input$reference_treatment,
                          common$logger)

    # LOAD INTO COMMON ####
    common$wrangled_data <- result$wrangled_data
    common$disconnected_indices <- result$disconnected_indices
    common$main_connected_data <- result$main_connected_data
    common$non_covariate_data_all <- result$non_covariate_data_all
    common$bugsnet_all <- result$bugsnet_all
    common$freq_all <- result$freq_all
    common$reference_treatment_all <- input$reference_treatment
    common$treatment_df <- result$treatment_df
    common$outcome_measure <- input$outcome_measure
    common$ranking_option <- input$rankopts
    common$logger %>% writeLog(type = "complete", "Data has been defined")

    # METADATA ####
    common$meta$setup_define$used <- TRUE
    common$meta$setup_define$reference_treatment <- input$reference_treatment
    common$meta$setup_define$rankopts <- input$rankopts
    common$meta$setup_define$outcome_measure <- input$outcome_measure

    # TRIGGER
    trigger("setup_define")
    show_table(parent_session)
    close_loading_modal()
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      reference_treatment = input$reference_treatment,
      rankopts = input$rankopts,
      outcome = input$outcome)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectizeInput(session, "reference_treatment", selected = state$reference_treatment)
      updateRadioButtons(session, "rankopts", selected = state$rankopts)
      updateRadioButtons(session, "outcome", selected = state$outcome)
    }
  ))
})
}


setup_define_module_rmd <- function(common){ list(
  setup_define_knit = !is.null(common$meta$setup_define$used),
  setup_define_reference_treatment = common$meta$setup_define$reference_treatment,
  setup_define_rankopts = common$meta$setup_define$rankopts,
  setup_define_outcome_measure = common$meta$setup_define$outcome_measure)
  # Variables used in the module's Rmd code
}

