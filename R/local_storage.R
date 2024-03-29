
#' @title Local Storage
#' 
#' @description
#' Class for accessing the browser local storage from within a *Shiny* app.
#' 
LocalStorage <- R6::R6Class(
  classname = "Local Storage",
  public = list(
    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      # Get root session so that storage is not tied to module namespace
      private$session <- shiny::getDefaultReactiveDomain()$rootScope()
      private$storage <- shiny::reactive({
        private$session$input$storage
      })
    },
    #' @description
    #' Write a value to the browser local storage. This will overwrite the value if it is already stored.
    #' 
    #' @param id (`character(1)`)\cr
    #'   Name of the value to store.
    #' @param value (`any`)\cr
    #'   Value to store.
    SetStoredValue = function(id, value) {
      private$session$sendCustomMessage(
        "add-to-storage", 
        list(
          id = id,
          value = as.character(jsonlite::toJSON(value))
        )
      )
    },
    #' @description
    #' Remove a value from the browser local storage.
    #' 
    #' @param id (`character(1)`)\cr
    #'   Name of the value to remove
    RemoveStoredValue = function(id) {
      private$session$sendCustomMessage(
        "remove-from-storage", 
        list(
          id = id
        )
      )
    },
    #' @description
    #' Retrieve a value from the browser local storage. This does not take a reactive dependency.
    #' 
    #' @param id (`character(1)`)\cr
    #'   Name of the value to retrieve.
    #'   
    #' @return
    #'   Value if stored in local storage, NULL if not present.
    GetStoredValue = function(id) {
      if (!is.null(isolate(private$storage()))) {
        value <- isolate(private$storage())[[id]]
        return(jsonlite::fromJSON(value))
      }
      return(NULL)
    },
    #' @description
    #' Retrieve the IDs of all values in the browser local storage. This does not take a reactive dependency.
    #'   
    #' @return
    #'   Vector of store item IDs, or NULL if storage is not available.
    GetStoredIds = function() {
      if (!is.null(isolate(private$storage()))) {
        return(isolate(names(private$storage())))
      }
      return(NULL)
    }
  ),
  private = list(
    #' @field session
    #' Root Shiny session.
    session = NULL,
    #' @field storage
    #' Reactive containing list of stored values.
    storage = NULL
  )
)

#' Include browser local storage access in the app. Add this anywhere within the app UI.
#'
#' @return Script for accessing browser local storage.
IncludeLocalStorage <- function() {
  singleton(
    tags$script(src = "storage.js")
  )
}
