####################### #
# MISC #
####################### #
#' @title printVecAsis
#' @description For internal use. Print objects as character string
#' @param x object to print
#' @return A character string to reproduce the object
#' @keywords internal
#' @export
printVecAsis <- function(x) {
  if (is.numeric(x) && length(x) == 1){
    return(x)
  } else {
    utils::capture.output(dput(x))
  }
}

#' @title Spurious package call to avoid note of functions outside R folder
#' @description For internal use.
#' @param x x
#' @keywords internal
#' @export
spurious <- function(x) {
  DT::renderDataTable(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinyjs::disable(x)
  return()
}

####################### #
# SHINY LOG #
####################### #

#' @title writeLog
#' @description For internal use. Add text to a logger
#' @param logger The logger to write the text to. Can be NULL or a function
#' @param ... Messages to write to the logger
#' @param type One of "default", "info", "error", "warning"
#' @keywords internal
#' @export
writeLog <- function(logger, ..., type = "default") {
  if (is.null(logger)) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == "starting") {
      pre <- paste0(icon("clock", class = "log_start"), " ")
    } else if (type == "complete") {
      pre <- paste0(icon("check", class = "log_end"), " ")
    } else if (type == "info") {
      if (nchar(...) < 200){
        shinyalert::shinyalert(..., type = "info")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "info")
      }
      pre <- paste0(icon("info", class = "log_info"), " ")
    } else if (type == "error") {
      if (nchar(...) < 200){
        shinyalert::shinyalert(...,
                               type = "error")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "error")
      }
      pre <- paste0(icon("xmark", class = "log_error"), " ")
    } else if (type == "warning") {
      if (nchar(...) < 200){
        shinyalert::shinyalert(...,
                               type = "warning")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "warning")
      }
      pre <- paste0(icon("triangle-exclamation", class = "log_warn"), " ")
    }
    newEntries <- paste0("<br>", pre, ..., collapse = "")
    logger(paste0(logger(), newEntries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}


#' Utility function for checking classes of function parameters
#'
#' @param params character. Vector of parameters to check
#' @param classes character. Vector of classes for each parameter
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window or returned as errors.
#' @return None - called for side effects
#' @keywords internal
check_param_classes <- function(params, classes, logger){
  for (i in seq_along(params)) {
    if (!inherits(get(params[i], envir = parent.frame()), classes[i])) {
      logger %>% writeLog(type = "error", paste0(params[i], " must be of class ", classes[i]))
      return()
    }
  }
}

####################### #
# LOADING MODAL #
####################### #

#' @title show_loading_modal
#' @description For internal use. Show a modal when something is loading
#' @param message The message to be displayed to the user
#' @keywords internal
#' @export

show_loading_modal <- function(message){
  shinybusy::show_modal_spinner(
    spin = "self-building-square",
    color = "#446e9b",
    text = message
  )
}
#' @title close_loading_modal
#' @description For internal use. Close the modal once loading is complete
#' @param session The session object passed to function given to shinyServer.
#' @keywords internal
#' @export

close_loading_modal <- function (session = getDefaultReactiveDomain())
{
  session$sendModal("remove", NULL)
}

####################### #
# CHANGING TABS #
####################### #

#' @title show_results
#' @description For internal use. Switches the view to the Results tab
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_results <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Results")
  bslib::accordion_panel_close("collapse_table", TRUE, session = parent_session)
}

#' @title show_table
#' @description For internal use. Switches the view to the Table panel
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_table <- function(parent_session){
  bslib::accordion_panel_open("collapse_table", TRUE, session = parent_session)
}


####################### #
# PLOTTING #
####################### #

#' Write a plot to a .pdf, .png or .svg file.
#'
#' @param file The file to which to write.
#' @param type String containing the type of file to which to write.
#' @param renderFunction A function to render the plot.
#' @param height The height of the plot in inches for pdf, or user specified units for png.
#' @param width The width of the plot in inches for pdf, or user specified units for png.
#' @export
write_plot <- function(file, type, renderFunction, height = NULL, width = NULL) {
  if (type == "pdf") {
    pdf(file = file, height = height, width = width)
  }
  if (type == "png") {
    png(file = file, height = height, width = width, units = "in", res = 300)
  }
  if (type == "svg") {
    svg(file = file, height = height, width = width)
  }
  renderFunction()
  dev.off()
}

#' Calculate the pixel height of a forest plot for a given number of treatments
#'
#' @param notrt The number of treatments in the plot
#' @param title TRUE if the title is included in the plot
#' @return The height of the plot in pixels
#' @export
forest_height_pixels <- function(notrt, title=FALSE, annotation = FALSE) {    # input is total number of treatments and whether title is included in plot
  height <- 15 * (notrt - 1) + 60

  if (title) {
    height <- height + 100
  }

  if (annotation) {
    height <- height + 80
  }

  return(height)
}

#' Create a pair of download buttons
#'
#' @param id The id of the module
#' @return tagList of downloadButtons
#' @export
download_button_pair <- function(id){
  ns <- NS(id)
  div(class = "download_buttons",
    fluidRow(
      column(width = 6, downloadButton(ns("download_all"), "All studies")),
      column(width = 6, downloadButton(ns("download_sub"), "Selected studies excluded"))
    )
  )

}

