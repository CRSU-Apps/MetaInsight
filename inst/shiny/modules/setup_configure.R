setup_configure_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectizeInput(ns("reference_treatment"), "Select reference treatment", choices = c()),
    radioButtons(ns("outcome_measure"), label = "Outcome for continuous data:",
                 choices = c("Mean Difference (MD)" = "MD", "Standardised Mean Difference (SMD)" = "SMD")),
    radioButtons(ns("ranking_option"), label = "For treatment rankings, values lower than the mean are:",
                 choices = c("Desirable" = "good", "Undesirable" = "bad")),
    numericInput(ns("seed"), add_tooltip("Seed value", "Normally suitable to leave as the default value"), value = 0, step = 1),
    actionButton(ns("run"), "Configure analysis", icon = icon("arrow-turn-down"))
  )
}

setup_configure_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  # Update UI depending on selections in previous module
  observe({
    watch("setup_load")
    req(common$loaded_data)
    treatments <- common$loaded_data$treatments$Label
    if (common$loaded_data$outcome == "continuous"){
      outcome_label <- "Outcome for continuous data:"
      outcome_choices <- c("Mean Difference (MD)" = "MD", "Standardised Mean Difference (SMD)" = "SMD")
      rank_label <- "For treatment rankings, values lower than the mean are:"
    } else {
      outcome_label <- "Outcome for binary data:"
      outcome_choices <- c("Odds Ratio (OR)" = "OR", "Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD")
      rank_label <- "For treatment rankings, ORs less than 1 are:"
    }
    updateSelectInput(session, "reference_treatment", choices = treatments,
                      selected = FindExpectedReferenceTreatment(treatments))
    updateRadioButtons(session, "outcome_measure", outcome_label, outcome_choices)
    updateRadioButtons(session, "ranking_option", rank_label,
                       selected = RankingOrder(common$loaded_data$outcome, !common$loaded_data$is_data_uploaded))
    updateNumericInput(session, "seed", value = common$seed)
    shinyjs::runjs("Shiny.setInputValue('setup_configure-ready', 'complete');")
  })

  observeEvent(input$run, {
    # WARNING ####

    if (is.null(common$loaded_data)) {
      common$logger |> writeLog(type = "error", go_to = "setup_load", "Please load data first")
      return()
    }

    if (!common$loaded_data$is_data_valid) {
      common$logger |> writeLog(type = "error", go_to = "setup_load", "Please upload valid data first")
      return()
    }

    # this is closed in setup_exclude once that has run
    show_loading_modal("Configuring analysis...")

    # FUNCTION CALL ####
    result <- setup_configure(common$loaded_data,
                              input$reference_treatment,
                              common$effects,
                              input$outcome_measure,
                              input$ranking_option,
                              as.numeric(input$seed),
                              common$logger)

    # LOAD INTO COMMON ####
    common$configured_data <- result

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
    common$meta$setup_configure$outcome_seed <- input$seed

    # TRIGGER
    trigger("setup_configure")

  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      reference_treatment = input$reference_treatment,
      ranking_option = input$ranking_option,
      outcome = input$outcome,
      seed = input$seed)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectizeInput(session, "reference_treatment", selected = state$reference_treatment, choices = common$treatment_df$Label)
      updateRadioButtons(session, "ranking_option", selected = state$ranking_option)
      updateRadioButtons(session, "outcome", selected = state$outcome)
      updateNumericInput(session, "seed", value = state$seed)
    }
  ))
})
}


setup_configure_module_rmd <- function(common){ list(
  setup_configure_knit = !is.null(common$meta$setup_configure$used),
  setup_configure_reference_treatment = common$meta$setup_configure$reference_treatment,
  setup_configure_ranking_option = common$meta$setup_configure$ranking_option,
  setup_configure_outcome_measure = common$meta$setup_configure$outcome_measure,
  setup_configure_effects = common$effects,
  setup_configure_seed = common$meta$setup_configure$seed)
}

