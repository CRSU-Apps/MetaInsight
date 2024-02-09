
PageNumbering <- R6::R6Class(
  classname = "Page Numbering",
  public = list(
    initialize = function() {
      private$root <- data.tree::Node$new("")
      private$current <- private$root
    },
    AddChild = function() {
      child <- private$current$AddChild(
        private$GetNextPageNumber()
      )
      return(private$BuildPageNumber(child))
    },
    DiveLevel = function() {
      current <- private$current
      if (length(current$children) == 0) {
        stop("Cannot dive level when node has no children")
      }
      private$current <- current$children[[length(current$children)]]
      invisible(self)
    },
    FloatLevel = function() {
      if (is.null(private$current$parent)) {
        stop("Cannot float level above root")
      }
      private$current <- private$current$parent
      invisible(self)
    }
  ),
  private = list(
    root = NULL,
    current = NULL,
    
    GetNextPageNumber = function() {
      # Numbers
      if (private$current$level %% 3 == 1) {
        if (length(private$current$children) == 0) {
          return(1)
        } else {
          last_child <- private$current$children[[length(private$current$children)]]
          return(as.character(as.integer(last_child$name) + 1))
        }
        
      # Letters
      } else if (private$current$level %% 3 == 2) {
        if (length(private$current$children) == 0) {
          return(letters[1])
        } else {
          last_child <- private$current$children[[length(private$current$children)]]
          return(letters[which(last_child$name == letters) + 1])
        }
      
      # Roman numerals
      } else {
        if (length(private$current$children) == 0) {
          return(tolower(as.roman(1)))
        } else {
          last_child <- private$current$children[[length(private$current$children)]]
          return(tolower(as.roman(as.integer(as.roman(last_child$name)) + 1)))
        }
      }
    },
    
    BuildPageNumber = function(node) {
      name = node$name
      
      while (!identical(node$parent, private$root)) {
        if (node$level %% 3 == 1) {
          name <- paste0(" ", name)
        } else if (node$level %% 3 == 2) {
          name <- paste0("-", name)
        }
        name <- paste0(node$parent$name, name)
        node <- node$parent
      }
      
      return(paste0(name, "."))
    }
  )
)
