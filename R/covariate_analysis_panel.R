#' Create the covariate analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
covariate_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      div(
        h2(textOutput(outputId = ns("subtitle"))),
        style = "display: inline-block; vertical-align: top; padding-right: 20pt;"
      ),
      div(
        conditionalPanel(
          condition = "output.valid_covariate",
          ns = ns,
          selectInput(
            inputId = ns("covariate_type_selection"),
            label = "",
            choices = c("Binary", "Continuous"),
            width = "120pt"
          )
        ),
        style = "display: inline-block;"
      ),
      div(
        conditionalPanel(
          condition = "output.inferred_type == 'Continuous'",
          ns = ns,
          div(
            tags$i(class = "fa-solid fa-circle-info"),
            title = "If your data is binary, the only allowed values are 0, 1, and NA",
            style = "color: red;"
          )
        ),
        style = "display: inline-block; vertical-align: 50%;"
      ),
      div(
        textOutput(outputId = ns("error_message_box")),
        style = "display: inline-block; color: red; font-style: italic; font-weight: bold; padding-right: 20pt;"
      ),
      covariate_value_panel_ui(id = ns("covariate_value")),
      conditionalPanel(
        condition = "output.valid_covariate",
        ns = ns,
        # Meta-regression UI should be placed here
      )
    )
  )
}

#' Create the covariate analysis server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
covariate_analysis_panel_server <- function(id, all_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    covariate_title <- reactive({
      FindCovariateNames(all_data())[1]
    })
    
    covariate_name <- reactive({
      GetFriendlyCovariateName(covariate_title())
    })
    
    covariate_type <- reactive({
      input$covariate_type_selection
    })
    
    output$subtitle <- renderText({
      return(glue::glue("Covariate: {covariate_name()}"))
    })
    
    error_message <- reactiveVal("")
    output$error_message_box <- renderText({ error_message() })
    
    inferred_type <- reactiveVal()
    
    observe({
      tryCatch(
        {
          inferred_type <- ValidateAndInferCovariateType(all_data(), covariate_title())
          shiny::updateSelectInput(inputId = "covariate_type_selection", selected = inferred_type)
          inferred_type(inferred_type)
          if (inferred_type == "Continuous") {
            shinyjs::disable(id = "covariate_type_selection")
          } else {
            shinyjs::enable(id = "covariate_type_selection")
          }
          error_message("")
        },
        error = function(exptn) {
          inferred_type(NULL)
          error_message(exptn$message)
        }
      )
    })
    
    output$valid_covariate <- reactive({ error_message() == "" })
    outputOptions(x = output, name = "valid_covariate", suspendWhenHidden = FALSE)
    
    output$inferred_type <- reactive({ inferred_type() })
    outputOptions(x = output, name = "inferred_type", suspendWhenHidden = FALSE)
    
    default_covariate_value <- reactiveVal()
    
    covariate_value = covariate_value_panel_server(
      id = "covariate_value",
      covariate_type = reactive({ input$covariate_type_selection }),
      covariate_data = reactive({ all_data()[[covariate_title()]] }),
      default_covariate_value = default_covariate_value
    )
    
    # Meta-regression server should be placed here, populating the default_covariate_value
  })
}
