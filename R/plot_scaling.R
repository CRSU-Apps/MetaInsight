#' Function to decide pixel size of plots based on the number of studies
#' 
#' @param studies The number of studies to be plotted (a positive int)
#' @return Height or width in pixels for png plots (a positive int)

calculate_plot_pixel <- function(studies) {
  ifelse(studies <=20, 400, 400 + 15*(studies-20))
}

#' Function to decide inch size of pdf plots based on the number of studies
#' 
#' @param studies The number of studies to be plotted (a positive int)
#' @return Height or width in inches for pdf plots (a positive int)

calculate_plot_pdf <- function(studies) {
  ifelse(studies <=25, 6, 6 + 0.2*(studies-25))
}