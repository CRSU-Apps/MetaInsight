setup_configure_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectizeInput(ns("reference_treatment"), "Select reference treatment", choices = c()),
    radioButtons(ns("outcome_measure"), label = "Outcome for continuous data:",
                 choices = c("Mean Difference (MD)" = "MD", "Standardised Mean Difference (SMD)" = "SMD")),
    radioButtons(ns("ranking_option"), label = "For treatment rankings, values lower than the mean are:",
                 choices = c("Desirable" = "good", "Undesirable" = "bad")),
    actionButton(ns("run"), "Configure analysis", icon = icon("arrow-turn-down"))
  )
}

setup_configure_module_server <- function(id, common, parent_session) {
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
    updateRadioButtons(session, "ranking_option", rank_label,
                       selected = RankingOrder(common$outcome, !common$is_data_uploaded))
  })

  observeEvent(input$run, {
    # WARNING ####

    if (is.null(common$data)) {
      common$logger |> writeLog(type = "error", "Please load data first")
      return()
    }

    if (!common$is_data_valid) {
      common$logger |> writeLog(type = "error", "Please upload valid data first")
      return()
    }

    # this is closed in setup_exclude once that has run
    show_loading_modal("Configuring analysis...")

    # FUNCTION CALL ####
    result <- setup_configure(common$data,
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
    common$reference_treatment_all <- result$reference_treatment
    common$treatment_df <- result$treatment_df
    common$outcome_measure <- input$outcome_measure
    common$ranking_option <- input$ranking_option

    # update with cleaned ids
    updateSelectInput(session, "reference_treatment", choices = common$treatment_df$Label,
                      selected = result$reference_treatment)

    common$logger |> writeLog(type = "complete", "The analysis has been configured")

    # METADATA ####
    common$meta$setup_configure$used <- TRUE
    # this is the uncleaned version
    common$meta$setup_configure$reference_treatment <- input$reference_treatment
    common$meta$setup_configure$ranking_option <- input$ranking_option
    common$meta$setup_configure$outcome_measure <- input$outcome_measure

    # TRIGGER
    trigger("setup_configure")
    show_table(parent_session)

  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      reference_treatment = input$reference_treatment,
      ranking_option = input$ranking_option,
      outcome = input$outcome)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectizeInput(session, "reference_treatment", selected = state$reference_treatment, choices = common$treatment_df$Label)
      updateRadioButtons(session, "ranking_option", selected = state$ranking_option)
      updateRadioButtons(session, "outcome", selected = state$outcome)
    }
  ))
})
}


setup_configure_module_rmd <- function(common){ list(
  setup_configure_knit = !is.null(common$meta$setup_configure$used),
  setup_configure_reference_treatment = common$meta$setup_configure$reference_treatment,
  setup_configure_ranking_option = common$meta$setup_configure$ranking_option,
  setup_configure_outcome_measure = common$meta$setup_configure$outcome_measure)
  # Variables used in the module's Rmd code
}

