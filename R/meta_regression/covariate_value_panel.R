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
        sliderInput(
          inputId = ns("slider"),
          label = NULL,
          min = 0,
          max = 1,
          value = 0
        ),
        style = "vertical-align: bottom; padding-right: 10pt;"
      ),
      .CreateInlineBlock(
        numericInput(
          inputId = ns("numeric"),
          label = NULL,
          min = 0,
          max = 1,
          value = 0,
          width = "100pt"
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
#' @param covariate_type Reactive containing the type of the covariate: either "Continuous" or "Binary".
covariate_value_panel_server <- function(id, covariate_type) {
  shiny::moduleServer(id, function(input, output, session) {
    
    covariate_value <- reactiveVal(0)
    
    # Update value and slider when numeric input changes
    observe({
      if (!is.null(covariate_type()) && covariate_type() == "Continuous") {
        covariate_value(input$numeric)
        shiny::updateSliderInput(inputId = "slider", value = input$numeric)
      }
    })
    
    # Update value and numeric input when slider changes
    observe({
      if (!is.null(covariate_type()) && covariate_type() == "Continuous") {
        covariate_value(input$slider)
        shiny::updateNumericInput(inputId = "numeric", value = input$slider)
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
  
    return(reactive({ covariate_value }))
  })
}
