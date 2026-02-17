#' Takes the configured data, removes any excluded studies and returns
#' subsets of the data to be passed to other functions.
#'
#' @param exclusions character. Vector of study names to exclude.
#' @inheritParams common_params
#' @return `configured_data` containing:
#'  \item{treatments}{dataframe. Treatment names and IDs}
#'  \item{reference_treatment}{character. The selected reference treatment}
#'  \item{connected_data}{dataframe. A subset of the data containing only connected studies}
#'  \item{covariate}{A list containing these items if covariate data exists or
#'  else empty:}
#'  \itemize{
#'   \item \code{cross}: Crosses
#'   \item \code{circle_open}: Open circles
#'   \item \code{none}: No symbols in which case only the plot of direct evidence is
#'  }
#'  \item{bugsnet}{dataframe. Processed data for bugsnet analyses created by `bugsnetdata`}
#'  \item{freq}{list. Processed data for frequentist analyses created by `frequentist()`}
#'  \item{outcome}{character. Whether the data is `binary` or `continuous`}
#'  \item{outcome_measure}{character. Outcome measure of the dataset.}
#'  \item{effects}{character. Whether the models are `fixed` or `random` effects}
#'  \item{ranking_option}{character. Whether higher values in the data are `good` or `bad`}
#'  \item{seed}{numeric. A seed value to be passed to models}
#' @export
#'
setup_exclude <- function(configured_data, exclusions, async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("configured_data"),
                            c("configured_data"), NULL)){
      return()
    }
  }

  if (!is.null(exclusions) && !inherits(exclusions, "character")){
    return(async |> asyncLog(type = "error", "error", "exclusions must be of class character"))
  }

  if (!is.null(exclusions) && all(!exclusions %in% configured_data$connected_data$Study)){
    return(async |> asyncLog(type = "error", "error", "exclusions must in the present in the loaded data"))
  }

  subsetted_data <- configured_data$non_covariate_data[!configured_data$non_covariate_data$Study %in% exclusions,]

  if (nrow(subsetted_data) == 0){
    return(async |> asyncLog(type = "error", "You have excluded all the studies"))
  }

  dewrangled_data <- ReinstateTreatmentIds(subsetted_data, configured_data$treatments)
  treatment_list <- FindAllTreatments(dewrangled_data)
  treatments <- CreateTreatmentIds(treatment_list, configured_data$reference_treatment)
  connected_data <- ReplaceTreatmentIds(dewrangled_data, treatments)
  non_covariate_data <- RemoveCovariates(connected_data)

  reference_treatment <- treatments$Label[treatments$Number == 1]

  bugsnet <- bugsnetdata(non_covariate_data,
                         configured_data$outcome,
                         treatments)

  freq <- frequentist(non_covariate_data,
                      configured_data$outcome,
                      treatments,
                      configured_data$outcome_measure,
                      configured_data$effects,
                      reference_treatment)

  output <- configured_data
  # delete unneeded and overwrite with new data
  output$non_covariate_data <- NULL
  output$wrangled_data <- NULL
  output$disconnected_indices <- NULL
  output$bugsnet <- bugsnet
  output$freq <- freq
  output$reference_treatment <- reference_treatment
  output$connected_data <- connected_data
  output$treatments <- treatments

  class(output) <- "configured_data"
  output
}

#' Produce an version of the `summary_study()` plot for use in the
#' interface for excluding studies. Inside the app this is interactive,
#' but it can also be rendered for non-interactive use.
#' @param exclusions character. Vector of excluded studies. Defaults to `NULL`,
#' but can be used to reset on loading
#' @param hover logical. Whether to shade the line on mouse hover and change
#' the cursor on clickable lines. Defaults to `FALSE`
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
setup_exclude_plot <- function(configured_data, exclusions = NULL, hover = FALSE){

  initial <- summary_study(configured_data, interactive = TRUE)
  svg_doc <- xml2::read_xml(initial)

  # Get viewBox dimensions
  svg_node <- xml2::xml_find_first(svg_doc, "//d1:svg", ns = c(d1 = "http://www.w3.org/2000/svg"))
  viewbox <- xml2::xml_attr(svg_node, "viewBox")
  values <- strsplit(viewbox, " ")[[1]] |> as.numeric()
  viewbox_x <- values[1]
  viewbox_width <- values[3]

  # add to svg id to allow targeting later
  xml2::xml_set_attr(svg_node, "id", "setup_exclude_interface")

  # Find all rect elements with stroke-width: 0.75
  rects <- xml2::xml_find_all(
    svg_doc,
    ".//d1:rect[contains(@style, 'stroke-width: 0.75')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Get rect y bounds
  rect_bounds <- lapply(rects, function(rect) {
    list(
      y_min = as.numeric(xml2::xml_attr(rect, "y")),
      y_max = as.numeric(xml2::xml_attr(rect, "y")) + as.numeric(xml2::xml_attr(rect, "height")),
      height = as.numeric(xml2::xml_attr(rect, "height")),
      element = rect
    )
  })

  # Find all other elements (not rects with stroke-width 0.75, not in defs)
  all_elements <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g/*[not(self::d1:rect[contains(@style, 'stroke-width: 0.75')])]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Helper function to get y coordinates from an element
  get_element_y_coords <- function(elem) {
    elem_name <- xml2::xml_name(elem)
    y_coords <- c()

    if (elem_name == "text") {
      y <- as.numeric(xml2::xml_attr(elem, "y"))
      # Only adjust for bullet point characters which use large font sizes
      if (grepl("•", xml2::xml_text(elem))) {
        style <- xml2::xml_attr(elem, "style")
        y_coords <- y - 12
      } else {
        y_coords <- y
      }
    } else if (elem_name == "line") {
      y_coords <- c(as.numeric(xml2::xml_attr(elem, "y1")),
                    as.numeric(xml2::xml_attr(elem, "y2")))
    } else if (elem_name %in% c("polyline", "polygon")) {
      points <- xml2::xml_attr(elem, "points")
      points_clean <- gsub(",", " ", points)
      vals <- as.numeric(strsplit(trimws(points_clean), "\\s+")[[1]])
      y_coords <- vals[seq(2, length(vals), 2)]
    } else if (elem_name == "rect") {
      y <- as.numeric(xml2::xml_attr(elem, "y"))
      height <- as.numeric(xml2::xml_attr(elem, "height"))
      y_coords <- c(y, y + height)
    }

    return(y_coords)
  }

  # Helper function to check if element overlaps with rect's y range
  is_inside_rect <- function(elem, rect_bound) {
    y_coords <- get_element_y_coords(elem)

    if (length(y_coords) == 0 || all(is.na(y_coords))) {
      return(FALSE)
    }

    y_min <- min(y_coords, na.rm = TRUE)
    y_max <- max(y_coords, na.rm = TRUE)

    # Check if element's y range overlaps with rect's y range
    return(y_min <= rect_bound$y_max && y_max >= rect_bound$y_min)
  }

  # Find parent group to add new groups to
  parent_group <- xml2::xml_find_first(
    svg_doc,
    ".//d1:g",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )

  # Track which elements have been assigned
  assigned_elements <- c()

  # Group elements by which rect they belong to
  for (i in seq_along(rect_bounds)) {
    rect_bound <- rect_bounds[[i]]

    # Create a new group element
    new_group <- xml2::xml_add_child(parent_group, "g")

    # Create a new rect with updated dimensions and style
    new_rect <- xml2::xml_add_child(new_group, "rect")
    xml2::xml_attr(new_rect, "x") <- viewbox_x
    xml2::xml_attr(new_rect, "y") <- rect_bound$y_min
    xml2::xml_attr(new_rect, "width") <- viewbox_width
    xml2::xml_attr(new_rect, "height") <- rect_bound$height

    # Find elements inside this rect's y range and collect text content
    first_text_content <- NULL

    for (j in seq_along(all_elements)) {
      if (!(j %in% assigned_elements)) {
        elem <- all_elements[[j]]
        if (is_inside_rect(elem, rect_bound)) {
          # Copy the element without namespace
          elem_copy <- xml2::xml_new_root(xml2::xml_name(elem))

          # Copy all attributes
          attrs <- xml2::xml_attrs(elem)
          for (attr_name in names(attrs)) {
            xml2::xml_attr(elem_copy, attr_name) <- attrs[[attr_name]]
          }

          # Copy text content if it's a text element
          if (xml2::xml_name(elem) == "text") {
            xml2::xml_text(elem_copy) <- xml2::xml_text(elem)
          }

          # Add to group
          xml2::xml_add_child(new_group, elem_copy)
          assigned_elements <- c(assigned_elements, j)


        }
      }
    }

    # add a clip path to each line - necessary because the RoB bullet points extend above and below
    clip_id <- paste0("clip-line", i)
    defs <- xml2::xml_find_first(svg_doc, ".//d1:defs", ns = c(d1 = "http://www.w3.org/2000/svg"))
    clip_path <- xml2::xml_add_child(defs, "clipPath")
    xml2::xml_attr(clip_path, "id") <- clip_id
    clip_rect <- xml2::xml_add_child(clip_path, "rect")
    xml2::xml_attr(clip_rect, "x") <- viewbox_x
    xml2::xml_attr(clip_rect, "y") <- rect_bound$y_min
    xml2::xml_attr(clip_rect, "width") <- viewbox_width
    xml2::xml_attr(clip_rect, "height") <- rect_bound$height
    xml2::xml_attr(new_group, "clip-path") <- paste0("url(#", clip_id, ")")

    # get study name from text element with lowest x coordinate
    text_elements <- xml2::xml_find_all(new_group, "text")
    x_coords <- sapply(text_elements, function(t) as.numeric(xml2::xml_attr(t, "x")))
    study_name <- xml2::xml_text(text_elements[[which.min(x_coords)]])
    xml2::xml_attr(new_group, "data-study-name") <- study_name

    # set style of rect
    xml2::xml_attr(new_rect, "style") <- glue::glue("stroke: none; opacity: 0; fill:#222222;")

    # set style of group, including cursor if hover is TRUE
    pointer <- ifelse(hover, "cursor:pointer;", "")
    opacity <- ifelse(study_name %in% exclusions, 0.3, 1)
    xml2::xml_attr(new_group, "style") <- glue::glue("{pointer} opacity: {opacity};")

    # Add ID
    xml2::xml_attr(new_group, "id") <- paste0("setup_exclude-line", i)
  }

  # Remove original rects
  xml2::xml_remove(rects)

  # Remove assigned elements
  if (length(assigned_elements) > 0) {
    xml2::xml_remove(all_elements[assigned_elements])
  }

  paste(svg_doc, collapse = "\n") |> HTML()

}
