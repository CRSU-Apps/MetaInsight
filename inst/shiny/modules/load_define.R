load_define_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectizeInput(ns("reference_treatment"), "Select reference treatment", choices = c()),
    conditionalPanel(
      condition= "output.metaoutcome == 'Continuous'",
      ns = ns,
      radioButtons(ns("outcomeCont"),
        label = "Outcome for continuous data:",
        choices = c(
          "Mean Difference (MD)" = "MD",
          "Standardised Mean Difference (SMD)" = "SMD"
        )
      )
    ),
    conditionalPanel(
      condition = "output.metaoutcome == 'Binary'",
      ns = ns,
      radioButtons(
        inputId = ns("outcomebina"),
        label = "Outcome for binary data:",
        choices = c(
          "Odds Ratio (OR)" = "OR",
          "Risk Ratio (RR)" = "RR",
          "Risk Difference (RD)" = "RD"
        )
      )
    ),
    radioButtons(
      inputId = ns("rankopts"),
      label = 'For treatment rankings, smaller outcome values (e.g. smaller mean values for continuous data, or ORs less than 1 for binary data) are:',
      choices = c(
        "Desirable" = "good",
        "Undesirable" = "bad"
      )
    ),
    radioButtons(
      inputId = ns("modelranfix"),
      label = "Model:",
      choices = c(
        "Random effect (RE)" = "random",
        "Fixed effect (FE)" = "fixed"
      )
    ),
    h3("Select studies to exclude:"),
    checkboxGroupInput(
      inputId = ns("exclusionbox"),
      label = NULL,
      choices = c()
    ),
    actionButton(ns("run"), "Define data")
  )
}

load_define_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  # Create the treatment list with the reference treatment being the first item in the data frame
  treatment_list <- reactive({
    return(CreateTreatmentIds(all_treatments(), input$reference_treatment))
  })

  observe({
    gargoyle::watch("load_load")
    req(common$treatment_list)
    treatments <- common$treatment_list
    updateSelectInput(session, "reference_treatment", choices = treatments,
                      selected = FindExpectedReferenceTreatment(treatments))
    updateRadioButtons(session, "rankopts", selected = RankingOrder(common$metaoutcome, !common$is_data_uploaded))
    updateCheckboxGroupInput(session, "exclusionbox", choices = unique(common$data$Study))
  })

  output$metaoutcome <- reactive({
    gargoyle::watch("load_load")
    common$metaoutcome
  })
  shiny::outputOptions(output, "metaoutcome", suspendWhenHidden = FALSE)


  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("load_define")
  })

  output$result <- renderText({
    gargoyle::watch("load_define")
    # Result
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

load_define_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

load_define_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

