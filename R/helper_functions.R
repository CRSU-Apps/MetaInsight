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
  cowplot::add_sub(x)
  DT::renderDataTable(x)
  grid::absolute.size(x)
  gt::adjust_luminance(x)
  knitr::all_labels(x)
  mirai::call_mirai(x)
  quarto::is_using_quarto(x)
  R6::is.R6Class(x)
  rintrojs::hintjs(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinyjs::disable(x)
  svglite::add_fonts(x)
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
# ADD TOOLTIP #
####################### #
#' Add a tooltip to the label of an input
#'
#' @param label character. The text to display initially
#' @param message character. The text to display in the tooltip
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

#' Write an svg plot to either a png, pdf or svg file
#'
#' @param svg html. containing the svg string, returned from `crop_svg()`
#' @param file character. The file to which to write.
#' @param type character. Type of file to which to write.
#' @export
write_plot <- function(svg, file, type) {

  xml <- xml2::read_html(svg)
  svg_node <- xml2::xml_find_first(xml, "//svg")
  viewbox <- xml2::xml_attr(svg_node, "viewbox")
  values <- strsplit(viewbox, " ")[[1]] |> as.numeric()
  width <- values[3] - values[1]
  height <- values[4] - values[2]

  if (type == "pdf") {
    rsvg::rsvg_pdf(charToRaw(svg), file, width, height)
  }
  if (type == "png") {
    rsvg::rsvg_png(charToRaw(svg), file, width * 3, height * 3)
  }
  if (type == "svg") {
    writeLines(svg, file)
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
#' For Bayesian plots this should be 'Compared to \{reference treatment\}'
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
  if (model$effects == "random") {   #SD and its 2.5% and 97.5%
    sd_mean <- round(sumresults$summaries$statistics["sd.d", "Mean"], digits = 2)
    sd_lowCI <- round(sumresults$summaries$quantiles["sd.d", "2.5%"], digits = 2)
    sd_highCI <- round(sumresults$summaries$quantiles["sd.d", "97.5%"], digits = 2)
  }   else {
    sd_mean <- 0
    sd_lowCI <- 0
    sd_highCI <- 0
  }
  if (model$effects=="random") {
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

#' Format xlimits to pretty values. Adapted from `gemtc::blobbogram()`
#'
#' @param x numeric. The value to format.
#' @param limit character. Either `min` or `max`
#' @param log.scale logical. Whether the values are on a log scale.
format_xlim <- function(x, limit, log.scale) {

  # Adapted from https://github.com/gertvv/gemtc/blob/b94d86a304eae57c8d16bb4aa8fc3f32155696e4/gemtc/R/blobbogram.R#L192

  # Scale transform and its inverse
  scale.trf <- if (log.scale) exp else identity
  scale.inv <- if (log.scale) log else identity

  # Round to a single significant digit, according to round.fun
  y <- scale.trf(x)
  p <- 10^floor(log10(abs(y)))
  if (limit == "min"){
    l <- scale.inv(floor(y / p) * p)
  }
  if (limit == "max"){
    l <- scale.inv(ceiling(y / p) * p)
  }

  if (is.na(l) || !is.finite(l)) x else l
}

#' Calculate a sensible step value to use in numericInput
#'
#' @param x numeric. The value to create a step for
#' @keywords internal
#' @export
format_step <- function(x){
  y <- 10 ^ floor(log10(abs(x) / 10))
  if (y == 0){
    y <- 0.1
  }
  y
}

#' Crops an svg object, by rendering it, locating the non-white pixels and
#' editing the svg viewBox property.
#'
#' @param svg xml_document. Output from `svglite::xmlSVG()`
#' @param margin numeric. The margin in pixels to leave around the edge
#' of the plot content. Defaults to 10.
#' @return html. The cropped svg
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

  # remove width and height
  xml2::xml_set_attr(svg_node, "width", NULL)
  xml2::xml_set_attr(svg_node, "height", NULL)

  # set namespace
  root <- xml2::xml_root(svg)
  xml2::xml_set_attr(root, "xmlns", "http://www.w3.org/2000/svg")
  xml2::xml_set_attr(root, "xmlns:xlink", "http://www.w3.org/1999/xlink")

  shiny::HTML(paste(svg, collapse = "\n"))
}

#' Put an svg in a container with buttons for fullscreen
#'
#' @param svg character. Output from `crop_svg()`
#' @param class character. Class of the container default `svg_container`
#' @param style character. Styles to add to the container
#' @keywords internal
#' @export
svg_container <- function(svg, class = "svg_container", style = ""){
  div(class = class, style = style,
      tags$button(
        class = "height-toggle-btn",
        onclick = "shinyjs.scrollingPlot(this)",
        # left-right arrow unicode
        "\u2194 Full width"
      ),
      tags$button(
        class = "fullscreen-btn",
        onclick = "shinyjs.fullscreenPlot(this.parentElement)",
        # diagonal arrow unicode
        "\u2922"
      ),
      svg)
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
  modules <- names(common$meta)
  # clear data
  common$reset()
  # blank outputs
  for (module in modules){
    gargoyle::trigger(module)
    # reset triggers
    session$userData[[module]](0)
  }
}

#' @title run_all
#' @description For internal use. Runs all modules for a given component, excluding the model and nodesplit modules.
#' @keywords internal
#' @param COMPONENTS character. named vector of all components
#' @param COMPONENT_MODULES list. Containing details of all modules
#' @param component character. The component to run all the modules of.
#' @param logger common$logger
#' @export
run_all <- function(COMPONENTS, COMPONENT_MODULES, component, logger){

  all_modules <- names(COMPONENT_MODULES[[component]])

  # exclude model and nodesplit modules
  if (component != "freq"){
    modules <- all_modules[-which(all_modules %in% c(glue::glue("{component}_model"), glue::glue("{component}_nodesplit")))]
  } else {
    modules <- all_modules
  }

  # workaround for regression modules where the run id differs
  # and append -run to module id to get run button ids
  modules <- paste0(ifelse(
    grepl("regression", modules),
    paste0(modules, "-", gsub("_regression", "", modules)),
    modules
  ), "-run")

  # click the run buttons
  shinyjs::runjs(
    paste(
      sprintf("$('#%s').click();", modules),
      collapse = "\n"
    )
  )

  # message for logger
  full_component <- names(COMPONENTS[COMPONENTS == component])

  if (component %in% c("bayes", "baseline", "covariate")){
    nodesplit_message <- " apart from the nodesplit module"
  } else {
    nodesplit_message <- ""
  }

  logger |> writeLog(type = "info",
                     glue::glue("Running all {full_component} modules{nodesplit_message}. This might
                                take several minutes and progress will appear in the logger."))

  invisible()
}


#' @title hide_and_show
#' @description For internal use. Hides content inside the <module_id>_div class
#' when a module has not been run and shows it once `trigger(module_id)` has
#' been called. The show option is `TRUE` by default but can be set to `FALSE`
#' to manually control when the content is displayed e.g.  if it should
#' be when an extendedtask completes instead of when the run button is
#' pressed.
#' @keywords internal
#' @param module_id character. The module identifier.
#' @param show logical. Whether to show the div when `trigger(module_id)` is
#' called
#' @export
hide_and_show <- function(module_id, show = TRUE){
  observe({
    gargoyle::watch(module_id)
    if (gargoyle::watch(module_id) == 0){
      shinyjs::hide(selector = glue::glue(".{module_id}_div"))
    } else {
      if (show){
        shinyjs::show(selector = glue::glue(".{module_id}_div"))
      }
    }
  })
}
