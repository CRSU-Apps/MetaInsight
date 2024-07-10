
#' Create the panel for adding and removing treatments.
#'
#' @param id ID of the module.
#' @return Div containing the module UI.
add_remove_panel_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Add treatments
    selectInput(
      inputId = ns("add_comparator_dropdown"),
      label = .AddRegressionOptionTooltip(
        tags$html("Add treatment", tags$i(class="fa-regular fa-circle-question")),
        tooltip = "All treatments are compared to the reference treatment"
      ),
      choices = c(),
      selectize = FALSE
    ),
    actionButton(inputId = ns("add_comparator_btn"), label = "Add to plot"),
    div(
      style = "float: right;",
      actionButton(inputId = ns("add_all_btn"), label = "Add all")
    ),
    br(),
    br(),
    # Remove treatments
    selectInput(inputId = ns("remove_comparator_dropdown"), label = "Remove treatment", choices = c(), selectize = FALSE),
    actionButton(inputId = ns("remove_comparator_btn"), label = "Remove"),
    div(
      style = "float: right;",
      actionButton(inputId = ns("remove_all_btn"), label = "Remove all")
    )
  )
}

#' Create the server for adding and removing treatments.
#'
#' @param id ID of the module.
#' @param comparator_names Reactive containing vector of all treatment names.
#' @param reference_name Reactive containing reference treatment name.
#'
#' @return Reactive containing added treatment names.
add_remove_panel_server <- function(id, comparator_names, reference_name) {
  shiny::moduleServer(id, function(input, output, session) {
    
    added_comparators <- reactiveVal(c())
    # These reactive values are here to prevent overly chatty reactives from repeatedly clearing the
    # treatment drop-down boxes when not appropriate
    current_comparator_names <- reactiveVal()
    current_reference_name <- reactiveVal()
    
    # Update treatment list only if changed
    observe({
      if (!setequal(current_comparator_names(), comparator_names())) {
        current_comparator_names(comparator_names())
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
      added_comparators(character())
    }) |>
      bindEvent(current_reference_name(), current_comparator_names())
    
    available_to_add <- reactive({
      return(comparator_names()[(comparator_names() != reference_name()) & !(comparator_names() %in% added_comparators())])
    })
    
    # Add treatment on button click
    observe({
      added = unique(c(added_comparators(), input$add_comparator_dropdown))
      added_comparators(added)
    }) |>
      bindEvent(input$add_comparator_btn)
    
    # Add all treatments on button click
    observe({
      added = unique(c(added_comparators(), available_to_add()))
      added_comparators(added)
    }) |>
      bindEvent(input$add_all_btn)
    
    # Remove treatment on button click
    observe({
      added = added_comparators()[added_comparators() != input$remove_comparator_dropdown]
      added_comparators(added)
    }) |>
      bindEvent(input$remove_comparator_btn, ignoreNULL = FALSE)
    
    # Remove all treatments on button click
    observe({
      added_comparators(character())
    }) |>
      bindEvent(input$remove_all_btn, ignoreNULL = FALSE)
    
    # Update inputs on added treatments change
    observe({
      updateSelectInput(inputId = "add_comparator_dropdown", choices = available_to_add())
    })
    
    # Update inputs on added treatments change
    observe(priority = -1000, {
      updateSelectInput(inputId = "remove_comparator_dropdown", choices = added_comparators())
    })
    
    return(added_comparators)
  })
}
