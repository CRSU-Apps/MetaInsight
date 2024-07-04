#' Create a panel which is replaced with an error message when a condition is not met.
#'
#' @param id ID of the module
#' @param inner_ui_expression Expression for the UI function for this item.
#' 
#' @return Div containing the module
informed_conditional_panel_ui <- function(id, inner_ui_expression, args = list()) {
  ns <- NS(id)
  
  div(
    conditionalPanel(
      condition = "!output.supported",
      ns = ns,
      uiOutput(outputId = ns("error_message"))
    ),
    conditionalPanel(
      condition = "output.supported",
      ns = ns,
      eval(
        {
          substitute({
            inner_ui_expression
          })
        },
        envir = args
      )
    )
  )
}

#' Create the server functions to show and hide the panel and error message. This will hide the given UI and display an
#' error message when the condition is not met. Note that this will throw an error when called with both a text and UI expression.
#'
#' @param id ID of the module
#' @param condition Reactive containing the state of the condition (TRUE or FALSE).
#' @param error_message_text_expression Expression to create text to be shown to the user when the condition is not met.
#' @param error_message_ui_expression Expression to create UI elements to be shown to the user when the condition is not met.
#' @param inner_server_expression Expression for the server function for this item.
informed_conditional_panel_server <- function(
    id,
    condition,
    error_message_text_expression = NULL,
    error_message_ui_expression = NULL,
    inner_server_expression) {
  shiny::moduleServer(id, function(input, output, session) {
    
    if (is.null(error_message_text_expression) == is.null(error_message_ui_expression)) {
      stop("Informed conditional panel must have either a message text expression or a message UI expression, not both.")
    }
    
    output$error_message <- renderUI({
      if (!is.null(error_message_text_expression)) {
        text <- eval({
          substitute({
            error_message_text_expression
          })
        })
        return(
          div(
            h3(text),
            style = "color: red; font-style: italic; font-weight: bold;"
          )
        )
      } else {
        return(
          eval({
            substitute({
              error_message_ui_expression
            })
          })
        )
      }
    })
    
    output$supported <- condition
    shiny::outputOptions(x = output, name = "supported", suspendWhenHidden = FALSE)
    
    return(
      eval({
        substitute({
          inner_server_expression
        })
      })
    )
  })
}
