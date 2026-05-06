
#' @title Parameter Matcher
#' 
#' @description
#' Class for checking matches between sets of parameters.
ParameterMatcher <- R6::R6Class(
  classname = "ParameterMatcher",
  public = list(
    #' Set parameters to be matched.
    #' @param ... Parameters to set
    #' @return self
    SetParameters = function(...) {
      parameters <- list(...)
      for (name in names(parameters)) {
        private$parameters[[name]] <- parameters[[name]]
      }
      invisible(self)
    },
    #' Clear the parameters to be matched.
    #' @return self
    Clear = function() {
      private$parameters <- list()
      invisible(self)
    },
    #' Check whether all parameters match.
    #' @param ... All parameters to match.
    #' @return TRUE if all parameters match.
    Matches = function(...) {
      actual <- list(...)
      expected_names = names(private$parameters)
      actual_names = names(actual)
      if (length(expected_names) != length(actual_names)) {
        return(FALSE)
      }
      
      for (name in expected_names) {
        if (!name %in% actual_names) {
          return(FALSE)
        }
        if (!R.oo::equals(private$parameters[[name]], actual[[name]])) {
          return(FALSE)
        }
      }
      
      return(TRUE)
    }
  ),
  private = list(
    #' @field parameters
    #' List of parameters to match.
    parameters = list()
  )
)
