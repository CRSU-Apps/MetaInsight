library(cookies)
library(shiny)

google_analytics_header_ui <- function(id) {
  ns <- NS(id)
  add_cookie_handlers(
    div(
      uiOutput(outputId = ns("analytics_script"))
    )
  )
}

google_analytics_header_server <- function(id, app_name, google_analytics_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    gdpr_cookie_value <- reactive({
      cookie_value <- cookies::get_cookie(cookie_name = "analytics")
      return(cookie_value)
    })
    
    observeEvent(
      once = TRUE,
      gdpr_cookie_value,
      {
        if (is.null(gdpr_cookie_value())) {
          showModal(
            modalDialog(
              title = "GDPR Notice",
              easyClose = FALSE,
              p(
                tags$strong("In accordance with Data Protection legislation, we would like to inform you of the following before you use our app:"),
                "We collect your usage data within this app to perform analytics of usage and improve our app. By clicking",
                tags$i(tags$u("I consent")),
                "below, you consent to the use of data by us through Google Analytics."
              ),
              br(),
              actionButton(inputId = ns("accept"), label = "I consent"),
              actionButton(inputId = ns("reject"), label = "I do not consent"),
              footer = NULL
            )
          )
        }
      }
    )
    
    observeEvent(
      input$accept,
      {
        shiny::removeModal()
        set_cookie(
          cookie_name = "analytics",
          cookie_value = TRUE,
          expiration = 365
        )
      }
    )
    
    observeEvent(
      input$reject,
      {
        shiny::removeModal()
        set_cookie(
          cookie_name = "analytics",
          cookie_value = FALSE,
          expiration = 365
        )
      }
    )
    
    output$analytics_script <- renderUI({
      cookie_value <- gdpr_cookie_value()
      if (!is.null(cookie_value) && as.logical(cookie_value)) {
        return(
          tags$head(
            singleton(
              tags$script(
                stringr::str_replace(
                  readr::read_file("google_analytics2.js"),
                  "<<GOOGLE_ANALYTICS_ID>>",
                  google_analytics_id
                )
              )
            )
          )
        )
      }
      return(NULL)
    })
  })
}
