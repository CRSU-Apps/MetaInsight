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
  .CreateInlineBlock(
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
            style = "color: red; font-style: italic; font-weight: bold; padding-left: 10pt"
          )
        )
      )
    ),
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
    ),
    style = "vertical-align: 50%;"
  )
}

#' Create the covariate value panel server.
#'
#' @param id ID of the module.
#' @param covariate_data Reactive containing vector of all covariate values.
#' @param default_covariate_value Reactive containing the default covariate value.
#' @param covariate_type Reactive containing the type of the covariate: either "Continuous" or "Binary".
covariate_value_panel_server <- function(id, covariate_type, covariate_data, default_covariate_value) {
  shiny::moduleServer(id, function(input, output, session) {
    
    min_value <- reactive({
      if (is.null(covariate_data())) {
        return(NULL)
      }
      return(min(covariate_data()))
    })
    
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
      if (is.null(default_covariate_value())) {
        value <- (min_value() + max_value()) / 2
      } else {
        value <- default_covariate_value()
      }
      
      shiny::updateNumericInput(inputId = "numeric", value = value, step = step)
    })
    
    covariate_value <- reactiveVal(0)
    
    # Update value and slider when numeric input changes
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
    
    output$covariate_type <- reactive({ covariate_type() })
    outputOptions(x = output, name = "covariate_type", suspendWhenHidden = FALSE)
    
    output$extrapolated <- reactive({ covariate_value() < min_value() || covariate_value() > max_value() })
    outputOptions(x = output, name = "extrapolated", suspendWhenHidden = FALSE)
  
    return(reactive({ covariate_value() }))
  })
}
