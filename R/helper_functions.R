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
  cookies::add_cookie_handlers(x)
  DT::renderDataTable(x)
  grid::absolute.size(x)
  jsonlite::as_gzjson_b64(x)
  knitr::all_labels(x)
  markdown::html_format(x)
  mirai::call_mirai(x)
  quarto::is_using_quarto(x)
  R6::is.R6Class(x)
  rintrojs::hintjs(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinyBS::addPopover(x)
  shinyjs::disable(x)
  svglite::add_fonts(x)
  zip::deflate(x)
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
#' @param go_to character. The id of a module to navigate to when the modal is closed.
#' Only used when `type = "error"`
#' @keywords internal
#' @export
writeLog <- function(logger, ..., type = "default", go_to = NULL) {
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

        # navigate to selected module on close
        callbackJS <- NULL
        if (!is.null(go_to)) {
          component <- stringr::str_split(go_to, "_")[[1]][1]
          callbackJS <- paste0("function() {
            document.querySelector('a[data-value=\"", component,"\"]').click();
            document.querySelector('input[value=\"", go_to, "\"').click();
          }")
        }

      if (nchar(...) < 200){
        shinyalert::shinyalert(...,
                               type = "error",
                               callbackJS = callbackJS)
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "error",
                               callbackJS = callbackJS)
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

#' @title asyncLog
#' @description For internal use. Similar to writeLog but for use inside async
#' functions
#' @param async Whether the function is being used asynchronously
#' @param ... Messages to write to the logger
#' @param type One of `default`, `info`, `error`, `warning`
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
asyncLog <- function(async, ..., type = "default"){
  if (!async) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else {
    return(as.character(...))
  }
}

#' Utility function for checking classes of function parameters
#'
#' @param params character. Vector of parameters to check
#' @param classes character. Vector of classes for each parameter
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window or returned as errors.
#' @return `TRUE` if any errors are found. `FALSE` if not
#' @keywords internal
check_param_classes <- function(params, classes, logger){
  for (i in seq_along(params)) {
    if (!inherits(get(params[i], envir = parent.frame()), classes[i])) {
      logger |> writeLog(type = "error", paste0(params[i], " must be of class ", classes[i]))
      return(TRUE)
    }
  }
  return(FALSE)
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
# LOADING SPINNER #
####################### #
#' @title loading_spinner
#' @description For internal use. Show a loading spinner when an output is recalculating.
#' Visibility is toggled using shinyjs::show(selector = ".class"). Note that you can use
#' multiple classes and they can be shown / hidden with independent triggers i.e. use one
#' to hide and a different one to show
#' @param class character
#' @keywords internal
#' @export
loading_spinner <- function(class) {
  div(class = class, style = "display: none;", # initially hidden
    div(class = "shiny-spinner-output-container",
      div(class = "load-container",
        div(class = "loader"))
))}


####################### #
# ADD TOOLTIP #
####################### #
#' @keywords internal
#' @export
add_tooltip <- function(label, message){
  span(label, tooltip(icon("circle-question"), message))
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
# SUPPRESS JAGS OUTPUT #
####################### #

#' @title suppress_jags_output
#' @description For internal use. Stop JAGS output appearing in the console /
#' html reports by wrapping model functions
#' @param expr The model function to be used
#' @keywords internal
suppress_jags_output <- function(expr) {
  null_device <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  sink(null_device)
  on.exit(sink())  # Ensure output is restored even if an error occurs
  force(expr)
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
    grDevices::pdf(file = file, height = height, width = width)
  }
  if (type == "png") {
    grDevices::png(file = file, height = height, width = width, units = "in", res = 300)
  }
  if (type == "svg") {
    grDevices::svg(file = file, height = height, width = width)
  }
  renderFunction()
  grDevices::dev.off()
}

#' Write an svg plot to either a png, pdf or svg file
#'
#' @param file character. The file to which to write.
#' @param type character. Type of file to which to write.
#' @param svg list. Containing the svg string, width and height returned from `crop_svg()`
#' @export
write_svg_plot <- function(file, type, svg) {
  if (type == "pdf") {
    rsvg::rsvg_pdf(charToRaw(svg$svg), file, svg$width, svg$height)
  }
  if (type == "png") {
    rsvg::rsvg_png(charToRaw(svg$svg), file, svg$width * 3, svg$height * 3)
  }
  if (type == "svg") {
    writeLines(svg$svg, file)
  }
}

#' Calculate the height in inches of a forest plot for a given number of treatments
#'
#' @param notrt The number of treatments in the plot
#' @param title TRUE if the title is included in the plot
#' @param annotation TRUE if an annotation is included in the plot
#' @return The height of the plot in pixels
#' @export
forest_height <- function(notrt, title=FALSE, annotation = FALSE) {
  # original calculations are pixel-based
  height <- 15 * (notrt - 1) + 60

  if (title) {
    height <- height + 100
  }

  if (annotation) {
    height <- height + 80
  }

  # return in inches at 72 dpi
  return(height / 72)
}


#' Calculate the width in inches of a forest plot for a given label.
#'
#' @param n_chars The number of characters in the widest label.
#' For frequentist plots this should be the longest treatment name.
#' For Bayesian plots this should be 'Compared to {reference treatment}'
#' @return The width of the plot in inches
#' @export
forest_width <- function(n_chars) {
  5 + (n_chars / 10)
}



#' Create a pair of download buttons
#'
#' @param id The id of the module
#' @return tagList of downloadButtons
#' @export
download_button_pair <- function(id){
  ns <- NS(id)
  div(class = "download_buttons",
    bslib::layout_columns(
      downloadButton(ns("download_all"), "All studies"),
      downloadButton(ns("download_sub"), "Selected studies excluded")
    )
  )
}

#' Create text with the point estimate and 95% CrI of between-trial SD of treatment effects.
#'
#' @param model Output created by `bayes_model()`
#' @return Text with the point estimate and 95% CrI of between-trial SD of treatment effects (all 0 if fixed effects)
#' @export
CreateTauSentence <- function(model) {
  sumresults <- model$sumresults
  if (model$model_type == "random") {   #SD and its 2.5% and 97.5%
    sd_mean <- round(sumresults$summaries$statistics["sd.d", "Mean"], digits = 2)
    sd_lowCI <- round(sumresults$summaries$quantiles["sd.d", "2.5%"], digits = 2)
    sd_highCI <- round(sumresults$summaries$quantiles["sd.d", "97.5%"], digits = 2)
  }   else {
    sd_mean = 0
    sd_lowCI = 0
    sd_highCI = 0
  }
  if (model$model_type=="random") {
    if (model$outcome=="OR") {
      paste("Between-study standard deviation (log-odds scale):", sd_mean, "\n 95% credible interval:", sd_lowCI, ",", sd_highCI)
    } else if (model$outcome=="RR") {
      paste("Between-study standard deviation (log probability scale):", sd_mean, "\n 95% credible interval:", sd_lowCI, ",", sd_highCI)
    } else {
      paste("Between-study standard deviation:", sd_mean, "\n 95% credible interval:", sd_lowCI, ",", sd_highCI)
    }
  } else{
    if (model$outcome=="OR") {
      "Between-study standard deviation (log-odds scale) set at 0"
    } else if (model$outcome=="RR") {
      "Between-study standard deviation (log probability scale) set at 0"
    } else {
      "Between-study standard deviation set at 0"
    }
  }
}

#' Crops an svg object, by rendering it, locating the non-white pixels and
#' editing the svg viewBox property. The width and height are also returned
#' to facilitate rendering to other formats.
#'
#' @param svg xml_document. Output from `svglite::xmlSVG()`
#' @param margin numeric. The margin in pixels to leave around the edge
#' of the plot content. Defaults to 10.
#' @return List containing:
#'  \item{svg}{character. The cropped svg}
#'  \item{width}{numeric. The width of the viewBox in pixels}
#'  \item{height}{numeric. The height of the viewBox in pixels}
#' @export

crop_svg <- function(svg, margin = 10){

  pixel_data <- paste(svg, collapse = "\n") |>
    magick::image_read_svg() |>
    magick::image_data()

  # Create a matrix of pixels containing content
  is_content <- !(
    # white (all RGB channels > 250)
    (pixel_data[1,,] > 250 &
       pixel_data[2,,] > 250 &
       pixel_data[3,,] > 250)
  )

  # bodge to get around grey border pixels
  is_content[, 1] <- FALSE
  is_content[1, ] <- FALSE
  is_content[nrow(is_content), ] <- FALSE
  is_content[, ncol(is_content)] <- FALSE

  content_pixels <- which(is_content, arr.ind = TRUE)

  x_coords <- content_pixels[, 1]
  y_coords <- content_pixels[, 2]

  x_min <- min(x_coords)
  x_max <- max(x_coords)
  y_min <- min(y_coords)
  y_max <- max(y_coords)

  width <- x_max - x_min
  height <- y_max - y_min

  bbox <- list(
    x = x_min - margin,
    y = y_min - margin,
    width = width + (margin * 2),
    height = height + (margin * 2)
  )

  # update viewBox
  svg_node <- xml2::xml_find_first(svg, "//svg")
  xml2::xml_attr(svg_node, "viewBox") <- paste(bbox$x, bbox$y, bbox$width, bbox$height)

  # fix the background element
  total_width <- gsub("pt", "", xml2::xml_attr(svg_node, "width"))
  total_height <- gsub("pt", "", xml2::xml_attr(svg_node, "height"))
  rect_node <- xml2::xml_find_first(svg, "//rect[@width='100%']")
  xml2::xml_attr(rect_node, "width") <- total_width
  xml2::xml_attr(rect_node, "height") <- total_height

  # set namespace
  root <- xml2::xml_root(svg)
  xml2::xml_set_attr(root, "xmlns", "http://www.w3.org/2000/svg")
  xml2::xml_set_attr(root, "xmlns:xlink", "http://www.w3.org/1999/xlink")

  list(
    svg = paste(svg, collapse = "\n"),
    width = bbox$width,
    height = bbox$height)
}

####################### #
# RESET DATA #
####################### #

#' @title reset_data
#' @description For internal use. Clears the common structure of data and resets all plots etc.
#' @keywords internal
#' @param common The common data structure
#' @export
reset_data <- function(common, session){
  # clear data
  common$reset()
  # browser()
  # blank outputs
  all_triggers <- names(session$userData)
  for (trigger in all_triggers){
    trigger(trigger)
  }
  # reset triggers
  lapply(session$userData, function(f) f(0))
}

