
#' Create a formatted string for a vector. This is to be used in test method where using `!!` fails to give a useful output.
#' 
#' @param vector Vector to display
#' @return The formatted string for the vector
format_vector_to_string <- function(vector) {
  is_char = is.character(vector)
  start = ifelse(is_char, 'c("', 'c(')
  end = ifelse(is_char, '")', ')')
  sep = ifelse(is_char, '", "', ', ')
  return(paste0(start, paste0(vector, collapse = sep), end))
}