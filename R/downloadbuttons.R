## MetaInsight - downloadable files for data uploading instructions - code.


## Sizing functions for forest plots ##

#' Calculate the pixel height of a forest plot for a given number of treatments
#' 
#' @param notrt The number of treatments in the plot
#' @param title TRUE if the title is included in the plot
#' @return The height of the plot in pixels
BayesPixels <- function(notrt, title=FALSE) {    # input is total number of treatments and whether title is included in plot
  if (notrt <= 25) {
    height <- 420        # default for 25 or less treatments
  } else {
    height <- 15*(notrt-1) + 60
  }
  
  if (title) {
    height <- height + 100
  }
  
  return(height)
}


#' Calculate the inch height of a forest plot for a given number of treatments
#' 
#' @param notrt The number of treatments in the plot
#' @return The height of the plot in inches
BayesInch <- function(notrt) {
  if (notrt <= 25) {
    height <- 6
  } else {
    height <- 6 + 0.2*(notrt-25)
  }
  return(height)
}

#' Write some plot to a .pdf or .png file
#' 
#' @param file The file to which to write
#' @param type String containing the type of file to which to write
#' @param renderFunction A function to render the plot
write_to_pdf_or_png <- function(file, type, renderFunction, height = NULL, width = NULL, png_units = "px") {

  if (tolower(type) == "pdf") {
    pdf(file = file, height = height, width = width)
  } else {
    png(file = file, height = height, width = width, units = png_units, res = 72)
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
