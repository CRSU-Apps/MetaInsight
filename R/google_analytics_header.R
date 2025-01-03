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

#' @return A \code{shiny::tags$head} with the analytics script
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
    shiny::tags$head(
      # Singleton for only including it once
      shiny::singleton(
        # Use script tags
        tags$script(
          # Substitute in the GTag ID
          stringr::str_replace_all(
            readr::read_file("google_analytics.js"),
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
#' @return \code{shiny::tags$head} head tags for scripts
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
GdprUi <- function(id) {
  # Get Namespace
  ns <- shiny::NS(id)
  shiny::tags$head(
    # JS library for managing cookies
    shiny::tags$script(src = "js/js.cookie.min.js"),
    # Application JS for custom cookie messages to Shiny
    shiny::tags$script(src = "js/app.js"),
    # Analytics script output
    shiny::uiOutput(outputId = ns("analytics_script"))
  )
}

#' @title GDPR Module Server
#' @description Server for GDPR Module
#' @param id Namespace id
#' @param cookies a \code{shiny::reacive} for input$cookies from cookies js
#' @param google_analytics_id GTag ID (Google Analytics 4)
#' @param tab a \code{shiny::reactive} for tab events
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
GdprServer <- function(
    id,
    cookies,
    google_analytics_id,
    tab
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      
      # Get the parent session (to determine if running in HTTPS)
      parent_session <- shiny::getDefaultReactiveDomain()$rootScope()
      
      # Reactive Val to store whether or not the GDPR Notice has been accepted
      # Defaults to False
      is_analytics <- shiny::reactiveVal(FALSE)
      
      # Run once when cookies.js has been loaded
      shiny::observe({
        # If the accept cookie is null show the GDPR notice
        if (is.null(cookies()$accept)) {
          GdprAlert()
          # Else if the user has previously accepted the GDPR notice
        } else if (as.logical(cookies()$accept)) {
          # Debug Message
          print("Previously Accepted")
          # Include the analytics sctipt
          output$analytics_script <- shiny::renderUI(
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
      }) |> shiny::bindEvent(cookies(), once = TRUE)
      
      # Observe the GDPR notice response
      shiny::observe({
        # Debug Message
        print(input$cookie_accept)
        # List for 'accept cookie' either
        # TRUE if GDPR notice accepted or
        # FALSE if GDPR notice declined
        # N.B. this is an essential cookie
        # so exempt from a seperate cookie notice
        msg <- list(
          name = "accept",
          value = input$cookie_accept
        )
        # Check if running in HTTPS or HTTP
        if (
          shiny::isolate(
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
          output$analytics_script <- shiny::renderUI({
            AddAnalytics(google_analytics_id)
          })
        }
        # Set the Reactive Val to the response
        is_analytics(as.logical(input$cookie_accept))
        # Debug Message
        print("Stored Cookie")
        # Bind to GDPR response
      }) |> shiny::bindEvent(input$cookie_accept)
      
      # Analytic 4 records events
      # Therefore observe the tab changes
      shiny::observe({
        # Require the acceptance of the GDPR Notice
        shiny::req(
          is_analytics(),
          cancelOutput = TRUE
        )
        # Debug Message
        print(paste0("Recording ", tab()))
        # Send the tab to Google Analytics through a js event
        session$sendCustomMessage(
          "add-event",
          list(
            value = tab()
          )
        )
        # Bind to tab being changed.
      }) |> shiny::bindEvent(
        tab()
      )
      
    }
  )
}