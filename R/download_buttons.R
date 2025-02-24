## MetaInsight - downloadable files for data uploading instructions - code.


## Sizing functions for forest plots ##

#' Calculate the pixel height of a node-split forest plot for a given number of comparisons for on screen
#'
#' @param ncomp The number of comparisons in the plot
#' @return The height of the plot in pixels
NodePixels <- function(ncomp) {
 return(max(400, ncomp * 80)) # 5 comparisons fits within the default of 400 pixels
}

#' Calculate download height of a node-split forest plot for a given number of comparisons
#'
#' @param ncomp The number of comparisons in the plot
#' @param type Type of measure, "in" for inches, "px" for pixels
#' @return The height of the plot
NodeDownloadHeight <- function(ncomp, type) {
  if (type == "in") {
    height <- max(7, ncomp * 1.25) # 6 comparisons fits within the default of 7 inches
  } else if (type == "px") {
    height <- max(480, ncomp * 80) # 6 comparisons fits within the default of 480 pixels
  } else {
    return("Type needs to be 'in' or 'px'")
  }
  return(height)
}


#' Calculate the pixel height of a forest plot for a given number of treatments
#'
#' @param notrt The number of treatments in the plot
#' @param title TRUE if the title is included in the plot
#' @return The height of the plot in pixels
#' @export
BayesPixels <- function(notrt, title=FALSE, annotation = FALSE) {    # input is total number of treatments and whether title is included in plot
  # if (notrt <= 25) {
  #   height <- 420        # default for 25 or less treatments
  # } else {
  height <- 15 * (notrt - 1) + 60
  # }

  if (title) {
    height <- height + 100
  }

  if (annotation) {
    height <- height + 80
  }

  return(height)
}


#' Calculate the inch height of a forest plot for a given number of treatments
#'
#' @param notrt The number of treatments in the plot
#' @return The height of the plot in inches
#' @export
BayesInch <- function(notrt) {
  if (notrt <= 25) {
    height <- 6
  } else {
    height <- 6 + 0.2 * (notrt - 25)
  }
  return(height)
}

#' Write some plot to a .pdf or .png file.
#'
#' @param file The file to which to write.
#' @param type String containing the type of file to which to write.
#' @param renderFunction A function to render the plot.
#' @param height The height of the plot in inches for pdf, or user specified units for png.
#' @param width The width of the plot in inches for pdf, or user specified units for png.
#' @export
write_to_pdf_or_png <- function(file, type, renderFunction, height = NULL, width = NULL) {
  if (tolower(type) == "pdf") {
    pdf(file = file, height = height, width = width)
  } else {
    png(file = file, height = height, width = width, units = "in", res = 216)
  }
  renderFunction()
  dev.off()
}


#########################
### Tab 2 - Load data ###
#########################

#' Create a download handler for raw data
#'
#' @param metaoutcome function to get th outcome to decide which file to download
#' @param filename Name of file to which to download
#' @param continuous_file File to download if continuous
#' @param binary_file File to download if binary
#' @return The created download handler
create_raw_data_download_handler <- function(metaoutcome, filename, continuous_file, binary_file) {
  return(
    downloadHandler(
      filename = filename,
      content = function(file) {
        if (metaoutcome() == 'Continuous') {
          file.copy(continuous_file, file)
        } else {
          file.copy(binary_file, file)
        }
      }
    )
  )
}
