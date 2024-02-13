
google_analytics_header_ui <- function(id) {
  ns <- NS(id)
  tags$head(
    IncludeLocalStorage(),
    uiOutput(outputId = ns("analytics_script"))
  )
}

google_analytics_header_server <- function(id, app_name, google_analytics_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cookie_name = glue::glue("{app_name}_analytics")
    gdpr_cookie_value <- reactiveVal(NULL)
    storage <- LocalStorage$new()
    
    initial_value_observer <- observe({
      gdpr_cookie_value(storage$GetStoredValue(cookie_name))
      initial_value_observer$destroy()
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
        storage$SetStoredValue(id = cookie_name, value = TRUE)
        gdpr_cookie_value(TRUE)
      }
    )
    
    observeEvent(
      input$reject,
      {
        shiny::removeModal()
        storage$SetStoredValue(id = cookie_name, value = FALSE)
        gdpr_cookie_value(FALSE)
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
