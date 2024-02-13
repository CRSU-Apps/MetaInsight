
LocalStorage <- R6::R6Class(
  classname = "Local Storage",
  public = list(
    initialize = function() {
      private$storage <- reactive({
        private$session$input$storage
      })
    },
    UpdateStoredValue = function(id, value) {
      private$session$sendCustomMessage(
        "update-storage", 
        list(
          id = id,
          value = value
        )
      )
    },
    GetStoredValue = function(id) {
      if (!is.null(isolate(private$storage()))) {
        return(isolate(private$storage())[[id]])
      }
      return(NULL)
    }
  ),
  private = list(
    session = shiny::getDefaultReactiveDomain()$rootScope,
    storage = NULL
  )
)

IncludeLocalStorage <- function() {
  singleton(
    tags$script(src = "storage.js")
  )
}
