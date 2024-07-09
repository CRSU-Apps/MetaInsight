
#' Create the regression plot panel.
#'
#' @param id ID of the module.
#' @return Div containing the module UI.
add_remove_panel_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Add treatments
    selectInput(
      inputId = ns("add_treatment_dropdown"),
      label = .AddRegressionOptionTooltip(
        tags$html("Add treatment", tags$i(class="fa-regular fa-circle-question")),
        tooltip = "All treatments are compared to the reference treatment"
      ),
      choices = c(),
      selectize = FALSE
    ),
    actionButton(inputId = ns("add_treatment_btn"), label = "Add to plot"),
    div(
      style = "float: right;",
      actionButton(inputId = ns("add_all_btn"), label = "Add all")
    ),
    br(),
    br(),
    # Remove treatments
    selectInput(inputId = ns("remove_treatment_dropdown"), label = "Remove treatment", choices = c(), selectize = FALSE),
    actionButton(inputId = ns("remove_treatment_btn"), label = "Remove"),
    div(
      style = "float: right;",
      actionButton(inputId = ns("remove_all_btn"), label = "Remove all")
    )
  )
}

#' Create the regression plot server.
#'
#' @param id ID of the module.
#' @param data Reactive containing study data including covariate columns, in wide or long format.
#' @param covariate_title Reactive containing title of the covariate column in the data.
#' @param covariate_name Friendly name of chosen covariate.
#' @param model_output Reactive containing model results found by calling `CovariateModelOutput()` or `BaselineRiskModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_type Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive type of outcome (OR, RR, RD, MD or SD).
#' @param package package used to create the model. Either "gemtc" (default) or "bnma".
add_remove_panel_server <- function(id, treatment_names, reference_name) {
  shiny::moduleServer(id, function(input, output, session) {
    
    added_treatments <- reactiveVal(c())
    current_treatment_names <- reactiveVal()
    current_reference_name <- reactiveVal()
    
    # Update treatment list only if changed
    observe({
      if (!setequal(current_treatment_names(), treatment_names())) {
        current_treatment_names(treatment_names())
      }
    })
    
    # Update reference treatment only if changed
    observe({
      if (is.null(current_reference_name()) || current_reference_name() != reference_name()) {
        current_reference_name(reference_name())
      }
    })
    
    # Clear added treatments when treatment list or reference change
    observe({
      added_treatments(character())
    }) |>
      bindEvent(current_reference_name(), current_treatment_names())
    
    available_to_add <- reactive({
      return(treatment_names()[(treatment_names() != reference_name()) & !(treatment_names() %in% added_treatments())])
    })
    
    # Add treatment on button click
    observe({
      added = unique(c(added_treatments(), input$add_treatment_dropdown))
      added_treatments(added)
    }) |>
      bindEvent(input$add_treatment_btn)
    
    # Add all treatments on button click
    observe({
      added = unique(c(added_treatments(), available_to_add()))
      added_treatments(added)
    }) |>
      bindEvent(input$add_all_btn)
    
    # Remove treatment on button click
    observe({
      added = added_treatments()[added_treatments() != input$remove_treatment_dropdown]
      added_treatments(added)
    }) |>
      bindEvent(input$remove_treatment_btn, ignoreNULL = FALSE)
    
    # Remove all treatments on button click
    observe({
      added_treatments(character())
    }) |>
      bindEvent(input$remove_all_btn, ignoreNULL = FALSE)
    
    # Update inputs on added treatments change
    observe({
      updateSelectInput(inputId = "add_treatment_dropdown", choices = available_to_add())
    })
    
    # Update inputs on added treatments change
    observe(priority = -1000, {
      # print("Updating input")
      # print(added_treatments())
      updateSelectInput(inputId = "remove_treatment_dropdown", choices = added_treatments())
    })
    
    return(added_treatments)
  })
}
