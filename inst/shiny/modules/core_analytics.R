#' @title Get GDPR Message
#' @description Returns GDPR message to be displayed in HTML

#' @return A character vector containing html to be displayed
#' in the GDPR message box
#' @details HTML content of the GDPR message with link to
#' google privacy policy
#' @examples
#' \dontrun{
#' if(interactive()){
#'  GetGdprMessage()
#'  }
#' }
#' @rdname GetGdprMessage
GetGdprMessage <- function() {
  "
  <b>
  In accordance with Data Protection legislation,
  we would like to inform you of the following before you use our website:
  </b>
  <p>
  We collect your usage data within the MetaInsight app
  to perform analytics of usage and improve our app.
  By clicking Accept below,
  you consent to the use of data by us through Google Analytics.
  For details of policy,
  please check the
  <a href='https://policies.google.com/privacy?hl=en' target='_blank'>
  Google Privacy & Terms
  </a>
  .
  </p>"
}

#' @title GDPR Alert
#' @description a \code{shinyalert} for the GDPR notice

#' @return a \code{shinyalert} with Accept and Decline buttons
#' @details Displays a \code{shinyalert} for the GDPR notice
#' from \code{GetGdprMessage}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  GdprAlert()
#'  }
#' }
#' @seealso
#'  \code{\link[shinyalert]{shinyalert}}
#' @rdname GdprAlert
#' @importFrom shinyalert shinyalert
GdprAlert <- function() {
  shinyalert::shinyalert(
    title = "GDPR Notice",
    text = GetGdprMessage(),
    type = "info",
    html = TRUE,
    closeOnEsc = FALSE,
    confirmButtonText = "Accept",
    showCancelButton = TRUE,
    cancelButtonText = "Decline",
    inputId = "cookie_accept"
  )
  shinyjs::runjs("document.body.scrollTop = document.documentElement.scrollTop = 0;")
}

#' @title Add analytics code
#' @description Returns a code to render the analytics script
#' @param google_analytics_id GTag ID (Analytics 4)

#' @return A \code{tags$head} with the analytics script
#' @details imports the \code{google_analytics.js} script and
#' substitutes the GTag ID and returns the script
#' to be included in the \code{head}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  AddAnalytics("G-XXXXXX")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{renderUI}}, \code{\link[shiny]{reexports}}
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[readr]{read_file}}
#' @rdname AddAnalytics
#' @importFrom shiny renderUI tags singleton
#' @importFrom stringr str_replace_all
#' @importFrom readr read_file
AddAnalytics <- function(google_analytics_id) {
  return(
    # Return a head tag
    tags$head(
      # Singleton for only including it once
      singleton(
        # Use script tags
        tags$script(
          # Substitute in the GTag ID
          stringr::str_replace_all(
            readr::read_file(system.file("shiny", "www", "js", "google_analytics.js", package = "metainsight")),
            "<<GOOGLE_ANALYTICS_ID>>",
            google_analytics_id
          )
        )
      )
    )
  )
}

#' @title GDPR Module UI
#' @description UI for the GDPR Module
#' @param id Namespace id
#' @return \code{tags$head} head tags for scripts
#' @details UI for the GDPR Module
#' @examples
#' \dontrun{
#' if(interactive()){
#'  GdprUi("GDPR")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{NS}},
#'  \code{\link[shiny]{reexports}},
#'  \code{\link[shiny]{htmlOutput}}
#' @rdname GdprUi
#' @importFrom shiny NS tags uiOutput
core_analytics_module_ui <- function(id) {
  # Get Namespace
  ns <- NS(id)
  tags$head(
    # JS library for managing cookies
    tags$script(src = file.path("resources", "js", "js.cookie.min.js")),
    # Application JS for custom cookie messages to Shiny
    tags$script(src = file.path("resources", "js", "app.js")),
    # Analytics script output
    uiOutput(outputId = ns("analytics_script"))
  )
}

#' @title GDPR Module Server
#' @description Server for GDPR Module
#' @param id Namespace id
#' @param cookies a \code{reacive} for input$cookies from cookies js
#' @param google_analytics_id GTag ID (Google Analytics 4)
#' @param module a \code{reactive} for the selected module
#' @return \code{NULL}
#' @details Server module for GDPR to handle GDPR notice and cookies
#' @examples
#' \dontrun{
#' if(interactive()){
#'  GdprServer("gdpr")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{moduleServer}},
#'  \code{\link[shiny]{domains}},
#'  \code{\link[shiny]{reactiveVal}},
#'  \code{\link[shiny]{observe}},
#'  \code{\link[shiny]{renderUI}},
#'  \code{\link[shiny]{bindEvent}},
#'  \code{\link[shiny]{isolate}},
#'  \code{\link[shiny]{req}}
#'  \code{\link[magrittr]{character(0)}}
#' @rdname GdprServer
#' @importFrom shiny
#'  moduleServer
#'  getDefaultReactiveDomain
#'  reactiveVal
#'  observe
#'  renderUI
#'  bindEvent
#'  isolate
#'  req
core_analytics_module_server <- function(
    id,
    cookies,
    google_analytics_id,
    module
) {
  moduleServer(
    id,
    function(input,
             output,
             session) {

      # Get the parent session (to determine if running in HTTPS)
      parent_session <- getDefaultReactiveDomain()$rootScope()

      # Reactive Val to store whether or not the GDPR Notice has been accepted
      # Defaults to False
      is_analytics <- reactiveVal(FALSE)

      # Run once when cookies.js has been loaded
      observe({
        # If the accept_analytics cookie is null show the GDPR notice
        if (is.null(cookies()$accept_analytics)) {
          GdprAlert()
          # Else if the user has previously accepted the GDPR notice
        } else if (as.logical(cookies()$accept_analytics)) {
          # Debug Message
          print("Previously Accepted")
          # Include the analytics sctipt
          output$analytics_script <- renderUI(
            AddAnalytics(google_analytics_id)
          )
          # Set the Reacive Val to True (GDPR notice accepted)
          is_analytics(TRUE)
          # Else the user has not previously accepted the GDPR Notice
        } else {
          # Set the Reactive Val to False (GDPR notice declined)
          is_analytics(FALSE)
        }
        # Bind once to cookies.js being initialised
      }) |> bindEvent(cookies(), once = TRUE)

      # Observe the GDPR notice response
      observe({
        # Debug Message
        print(input$cookie_accept)
        # List for 'accept_analytics cookie' either
        # TRUE if GDPR notice accepted or
        # FALSE if GDPR notice declined
        # N.B. this is an essential cookie
        # so exempt from a separate cookie notice
        msg <- list(
          name = "accept_analytics",
          value = input$cookie_accept
        )
        # Check if running in HTTPS or HTTP
        if (
          isolate(
            parent_session$clientData$url_protocol
          ) == "https:"
        ) {
          # Debug Message
          print("Running in HTTPS")
          # If running in HTTPS set a secure cookie
          session$sendCustomMessage("cookie-set-secure", msg)
        } else {
          # Debug Message
          print("Running in HTTP")
          # If running in HTTP set a 'normal cookie'
          session$sendCustomMessage("cookie-set", msg)
        }
        # If the GDPR notice was accepted
        if (as.logical(input$cookie_accept)) {
          # Debug Message
          print("Adding Analytics")
          # Add the analytics script
          output$analytics_script <- renderUI({
            AddAnalytics(google_analytics_id)
          })
        }
        # Set the Reactive Val to the response
        is_analytics(as.logical(input$cookie_accept))
        # Debug Message
        print("Stored Cookie")
        # Bind to GDPR response
      }) |> bindEvent(input$cookie_accept)

      # Analytic 4 records events
      # Therefore observe the module changes
      observe({
        # Require the acceptance of the GDPR Notice
        req(
          is_analytics(),
          cancelOutput = TRUE
        )
        # Debug Message
        print(paste0("Recording ", module()))
        # Send the module to Google Analytics through a js event
        session$sendCustomMessage(
          "add-event",
          list(
            value = module()
          )
        )
        # Bind to module being changed.
      }) |> bindEvent(
        module()
      )

    }
  )
}
