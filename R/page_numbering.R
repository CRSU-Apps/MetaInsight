
#' @title Page Numbering
#' 
#' @description
#' Class for numbering pages in a logical manner.
PageNumbering <- R6::R6Class(
  classname = "PageNumbering",
  public = list(
    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      private$root <- data.tree::Node$new("")
      private$current <- private$root
    },
    #' Add a child at the current level. The pages will be numbered with:
    #' - Hindu-Arabic numerals (eg. 1, 2, 3, 4)
    #' - Latin alphabet letters (eg. a, b, c, d)
    #' - Roman numerals (eg. i, ii, iii, iv)
    #' These will be arranged in the repeating format "1b iii-...".
    #'
    #' @return Formatted page number
    AddChild = function() {
      child <- private$current$AddChild(
        private$GetNextPageNumber()
      )
      return(private$BuildPageNumber(child))
    },
    #' Create a sub-level within the current numbering level.
    #'
    #' @return self
    DiveLevel = function() {
      current <- private$current
      if (length(current$children) == 0) {
        stop("Cannot dive level when node has no children")
      }
      private$current <- current$children[[length(current$children)]]
      invisible(self)
    },
    #' Move up out of the current sub-level.
    #'
    #' @return self
    FloatLevel = function() {
      if (is.null(private$current$parent)) {
        stop("Cannot float level above root")
      }
      private$current <- private$current$parent
      invisible(self)
    }
  ),
  private = list(
    #' @field root
    #' The top-level for numbering the entire app.
    root = NULL,
    #' @field current
    #' The current numbering level.
    current = NULL,
    
    #' Find the next identifier at the current level.
    #'
    #' @return String of the next identifier at this level.
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
    
    #' Build the full page number for a given node. This will combine all parents in the repeating format "1b iii-...".
    #'
    #' @param node Node for which to build page number.
    #'
    #' @return Full formatted page number.
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
