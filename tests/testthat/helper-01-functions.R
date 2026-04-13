### FORMATTING #

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


### PLOTTING #

validate_plot <- function(file_path) {
  img <- magick::image_read(file_path)
  return(magick::image_info(img)$width > 0)  # Check if the image has a valid width
}

test_plot_downloads <- function(app, module, pair = TRUE) {
  if (pair){
    download_all_id <- paste0(module, "-download_all")
    download_sub_id <- paste0(module, "-download_sub")

    app$set_inputs("download_format" = "png")
    png_all <- app$get_download(download_all_id)
    png_sub <- app$get_download(download_sub_id)
    expect_gt(file.info(png_all)$size, 1000)
    expect_gt(file.info(png_sub)$size, 1000)
    expect_true(validate_plot(png_all))
    expect_true(validate_plot(png_sub))

    unlink(c(png_all, png_sub))

  } else {
    download_id <- paste0(module, "-download")

    app$set_inputs("download_format" = "png")
    png <- app$get_download(download_id)
    expect_gt(file.info(png)$size, 1000)
    expect_true(validate_plot(png))

    unlink(png)
  }
}

# similar to above, but for bayes modules which use submodules
test_bayes_plot_downloads <- function(app, module, plot_id, dual = TRUE) {
  download_all_id <- paste0(module, "-all-download", plot_id)

  app$set_inputs("download_format" = "png")
  png_all <- app$get_download(download_all_id)
  expect_gt(file.info(png_all)$size, 1000)
  expect_true(validate_plot(png_all))

  unlink(png_all)

}



### SHINYTEST2 #

# reload from a save file and close alert
reload_app <- function(app, path){
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = path)
  app$click("setup_reload-goLoad_session")
  app$wait_for_js("$('.sweet-alert.visible').length > 0")
  app$click(selector = ".confirm")
}

# click on the exclusions forest plot
click_setup_exclude <- function(app, study){
  app$run_js(glue::glue('
  var elem = document.querySelector(\'[data-study-name="{study}"]\');
  if (elem) {{
    elem.dispatchEvent(new MouseEvent("click", {{
      bubbles: true,
      cancelable: true,
      view: window
    }}));
  }}'))
}
