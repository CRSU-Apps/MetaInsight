
#' Create a formatted string for a vector. This is to be used in test method where using `!!` fails to give a useful output.
#'
#' @param vector Vector to display
#' @return The formatted string for the vector
format_vector_to_string <- function(vector) {
  return(capture.output(dput(vector)))
}

# removes igraph IDs from gemtc model outputs which are not reproducible
#' @param result gemtc model output
#' @return The model output with the igraph IDs removed
remove_igraph <- function(result){

  graph_to_comparable <- function(g) {
    list(
      vertices = igraph::V(g)$name,
      edges = igraph::as_edgelist(g),
      vertex_attributes = igraph::vertex_attr(g),
      edge_attributes = igraph::edge_attr(g),
      graph_attributes = igraph::graph_attr(g),
      directed = igraph::is_directed(g)
    )
  }

  result$mtcResults$model$tree <- graph_to_comparable(result$mtcResults$model$tree)
  result$mtcRelEffects$model$tree <- graph_to_comparable(result$mtcRelEffects$model$tree)
  result$rel_eff_tbl <- as.data.frame(result$rel_eff_tbl)

  result
}



# Function to extract text content from each SVG in HTML file
extract_svg_text_from_html <- function(html_file) {
  html_doc <- xml2::read_html(html_file)

  svg_nodes <- xml2::xml_find_all(html_doc, "//svg")

  svg_text_list <- lapply(svg_nodes, function(svg_node) {
    text_nodes <- xml2::xml_find_all(svg_node, ".//text")
    text_content <- xml2::xml_text(text_nodes)
    return(text_content)
  })

  return(svg_text_list)
}

extract_svg_text_from_svg <- function(svg){
  doc <- xml2::read_html(svg)
  text_nodes <- xml2::xml_find_all(doc, ".//text")
  xml2::xml_text(text_nodes)
}
