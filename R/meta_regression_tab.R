#' Create the meta-regression page.
#'
#' @param id ID of the module
#' @return Div containing the meta-regression page
meta_regression_tab_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      div(
        h2("Meta-Regression"),
        style = "display: inline-block;"
      ),
      div(
        style = "display: inline-block; padding-right: 20pt;"
      ),
      div(
        h3(textOutput(outputId = ns("subtitle"))),
        style = "display: inline-block;"
      ),
      div(
        textOutput(outputId = ns("error_message_box")),
        style = "color: red; font-style: italic; font-weight: bold;"
      ),
      conditionalPanel(
        condition = "!output.error_message",
        {
          selectInput(
            inputId = ns("covariate_type_selection"),
            label = "Covariate Type",
            choices = c("binary", "continuous")
          )
          # Meta-regression UI should be placed here
        }
      )
    )
  )
}

#' Create the meta-regression server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @return Reactive returning TRUE if a covariate is available, else FALSE
meta_regression_tab_server <- function(id, all_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    covariate_title <- shiny::reactive({
      FindCovariateNames(all_data())[1]
    })
    
    covariate_name <- shiny::reactive({
      GetFriendlyCovariateName(covariate_title())
    })
    
    output$subtitle <- shiny::renderText({
      return(glue::glue("Covariate: {covariate_name()}"))
    })
    
    error_message <- reactiveVal("")
    output$error_message <- reactive({ error_message() })
    outputOptions(x = output, name = "error_message", suspendWhenHidden = FALSE)
    
    output$error_message_box <- renderText({ error_message() })
    
    observe({
      tryCatch(
        {
          inferred_type <- InferCovariateType(all_data(), covariate_title())
          shiny::updateSelectInput(inputId = "covariate_type_selection", selected = inferred_type)
          error_message("")
        },
        error = function(exptn) {
          error_message(exptn$message)
        }
      )
    })
    
    return(reactive({ is.na(covariate_title()) }))
  })
}
