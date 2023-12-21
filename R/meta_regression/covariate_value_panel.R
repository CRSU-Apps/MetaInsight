.CreateInlineBlock <- function(..., style = NULL) {
  return(
    div(
      ...,
      style = paste0("display: inline-block; ", style)
    )
  )
}

#' Create the covariate value panel.
#'
#' @param id ID of the module.
#' @return Div containing the module UI.
covariate_value_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    title = "This value will be used by all regression analysis output",
    style = "padding-left: 5px;",
    .CreateInlineBlock(
      h4("Covariate value:")
    ),
    .CreateInlineBlock(
      # Continuous value input & warning if extrapolating outside of data range
      conditionalPanel(
        condition = "output.covariate_type == 'Continuous'",
        ns = ns,
        .CreateInlineBlock(
          numericInput(
            inputId = ns("numeric"),
            label = NULL,
            min = -.Machine$double.xmax,
            max = .Machine$double.xmax,
            value = 0,
            width = "100pt"
          )
        ),
        .CreateInlineBlock(
          conditionalPanel(
            condition = "output.extrapolated",
            ns = ns,
            div(
              "Covariate value outside data range",
              style = "color: orange; font-style: italic; font-weight: bold; padding-left: 10pt"
            )
          )
        )
      ),
      # Binary value input
      conditionalPanel(
        condition = "output.covariate_type == 'Binary'",
        ns = ns,
        div(
          .CreateInlineBlock("0", style = "padding-right: 10pt;"),
          shinyWidgets::materialSwitch(
            inputId = ns("toggle"),
            inline = TRUE
          ),
          .CreateInlineBlock("1")
        )
      )
    )
  )
}

#' Create the covariate value panel server.
#'
#' @param id ID of the module.
#' @param covariate_type Reactive containing the type of the covariate: either "Continuous" or "Binary".
#' @param covariate_data Reactive containing vector of all covariate values.
covariate_value_panel_server <- function(id, covariate_type, covariate_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Minimum covariate value in the data
    min_value <- reactive({
      if (is.null(covariate_data())) {
        return(NULL)
      }
      return(min(covariate_data()))
    })
    
    # Maximum covariate value in the data
    max_value <- reactive({
      if (is.null(covariate_data())) {
        return(NULL)
      }
      return(max(covariate_data()))
    })
    
    # Update the numeric input to the centre of the range, or the default value,
    # and the step to be a reasonable size of roughly 100 steps
    observe({
      range <- max_value() - min_value()
      log_val <- round(log10(range))
      step <- 10 ** (log_val - 2)
      value <- (min_value() + max_value()) / 2
      shiny::updateNumericInput(inputId = "numeric", value = value, step = step)
    })
    
    covariate_value <- reactiveVal(0)
    
    # Update value when numeric input changes
    observe({
      if (!is.null(covariate_type()) && covariate_type() == "Continuous") {
        covariate_value(input$numeric)
      }
    })
    
    # Update value when toggle input changes
    observe({
      if (!is.null(covariate_type()) && covariate_type() == "Binary") {
        covariate_value(ifelse(input$toggle, 1, 0))
      }
    })
    
    # Update the client code to display the correct value input for the covariate type
    output$covariate_type <- reactive({ covariate_type() })
    outputOptions(x = output, name = "covariate_type", suspendWhenHidden = FALSE)
    
    # Update the client code to inform the user when the covariate value is outside the range of the data
    output$extrapolated <- reactive({ covariate_value() < min_value() || covariate_value() > max_value() })
    outputOptions(x = output, name = "extrapolated", suspendWhenHidden = FALSE)
    
    return(debounce(r = reactive({ covariate_value() }), millis = 500))
  })
}
