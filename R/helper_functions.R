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
#' @noRd
spurious <- function(x) {
  cookies::add_cookie_handlers(x)
  cowplot::add_sub(x)
  DT::renderDataTable(x)
  gargoyle::watch(x)
  knitr::all_labels(x)
  mirai::call_mirai(x)
  quarto::is_using_quarto(x)
  R6::is.R6Class(x)
  rintrojs::hintjs(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinybusy::add_busy_bar(x)
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
#' @noRd
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
# SUPPRESS JAGS OUTPUT #
####################### #

#' @title suppress_jags_output
#' @description For internal use. Stop JAGS output appearing in the console /
#' html reports by wrapping model functions
#' @param expr The model function to be used
#' @noRd
suppress_jags_output <- function(expr) {
  null_device <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  sink(null_device)
  on.exit(sink())  # Ensure output is restored even if an error occurs
  force(expr)
}

####################### #
# PLOTTING #
####################### #

#' @title Write plots to a file
#' @description Write an svg plot to either a png, pdf or svg file.
#'
#' @param svg html. containing the svg string, returned from `crop_svg()`
#' @param file character. The file to which to write.
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' tmp <- tempfile(fileext = ".png")
#' summary_network(configured_data = configured_data,
#'                 style = "netgraph") |>
#'                 write_plot(tmp)
#' unlink(tmp)
#' @export
write_plot <- function(svg, file) {

  type <- tools::file_ext(file)
  if (!(type %in% c("pdf", "png", "svg"))){
    stop("file must have an extension of pdf, png or svg")
  }

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
#' @noRd
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
#' @noRd
forest_width <- function(n_chars) {
  5 + (n_chars / 10)
}

#' Create text with the point estimate and 95% CrI of between-trial SD of treatment effects.
#'
#' @param model Output created by `bayes_model()`
#' @return Text with the point estimate and 95% CrI of between-trial SD of treatment effects (all 0 if fixed effects)
#' @noRd
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
#' @keywords internal
#' @export
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
#' @param render_text logical. Whether to convert text to paths. Defaults to
#' `TRUE`.
#' @return html. The cropped svg
#' @noRd
crop_svg <- function(svg, margin = 10, render_text = TRUE){

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

  svg_string <- paste(svg, collapse = "\n")

  # convert <text> elements to <path>
  if (render_text){
    svg_string <- svg_string |>
      charToRaw() |>
      rsvg::rsvg_svg(NULL) |>
      rawToChar()
  }

  shiny::HTML(svg_string)
}

####################### #
# DIC TABLE #
####################### #

#' Create a summary table of deviance information criterion stats for Bayesian models
#'
#' @param dic dataframe of DIC stats from `baseline_model`, `bayes_model()`
#' or `covariate_model()`
#' @param analysis Whether the analysis is using all studies (`all`) or a subset (`sub`)
#' @export
dic_table <- function(dic, analysis = "all"){

  if (analysis == "all"){
    title = "Model fit for all studies"
  } else if (analysis == "sub"){
    title = "Model fit with selected studies excluded"
  } else {
    stop("analysis must be 'all' or 'sub'")
  }

  rownames(dic) <- c("Posterior mean deviance (Dbar)",
                     "Effective number of parameters (pD)",
                     "Dbar + pD = Deviance information criterion (DIC)",
                     "Study arms")

  dic |>
    gt::gt(rownames_to_stub = TRUE) |>
    gt::tab_header(title = title) |>
    gt::tab_options(column_labels.hidden = TRUE) |>
    gt::fmt_number(rows = c(1:3), decimals = 3) |>
    gt::fmt_number(rows = 4, decimals = 0)
}


