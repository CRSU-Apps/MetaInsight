
#' Create a formatted string for a vector. This is to be used in test method where using `!!` fails to give a useful output.
#'
#' @param vector Vector to display
#' @return The formatted string for the vector
format_vector_to_string <- function(vector) {
  return(capture.output(dput(vector)))
}
