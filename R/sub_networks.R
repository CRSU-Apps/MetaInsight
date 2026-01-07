
#' Create a graph where the nodes are treatments and the links are studies comparing those treatments.
#'
#' @param data Data for which to create graph.
#'
#' @return Created igraph object.
CreateGraph <- function(data) {
  # Find links
  links = c()
  for (study in unique(data$Study)) {
    study_treatments <- FindAllTreatments(data[data$Study == study, ])
    for (i in 1:(length(study_treatments) - 1)) {
      for (j in (i + 1):length(study_treatments)) {
        links <- c(links, study_treatments[i], study_treatments[j])
      }
    }
  }

  # Build network (Mathematical structure is called a 'graph')
  return(igraph::make_graph(links, directed = FALSE))
}

#' Determine if the network contains any splittable nodes. If not, state the reason.
#'
#' @param data The dataset from which to create the network.
#' @param treatments Vector of treatments in order.
#' @return List:
#'  - is_nodesplittable: TRUE or FALSE.
#'  - reason: Text describing the reason there are no splittable nodes, or NULL when is_nodesplittable == TRUE.
IsNodesplittable <- function(data, treatments) {
  graph <- CreateGraph(data = data)
  #If there are no loops then return FALSE
  if (igraph::is_acyclic(igraph::simplify(graph, remove.multiple = TRUE))) {
    return(
      list(
        is_nodesplittable = FALSE,
        reason = "There are no loops in the network."
      )
    )
  }
  #Loop through comparisons
  for (treatment1_index in 1:length(treatments)) {
    #The treatments that are compared directly to treatment1
    adjacent_to_treatment1 <- unique(
      igraph::adjacent_vertices(
        graph = graph,
        v = treatments[treatment1_index]
      )[[1]]
    )
    for (treatment2_index in (treatment1_index + 1):length(treatments)) {
      #Check treatment1 and treatment2 are directly compared
      if (treatments[treatment2_index] %in% names(adjacent_to_treatment1)) {
        #All paths from treatment1 to treatment2 (each of which is part of a loop from treatment1 to treatment2)
        paths <- igraph::all_simple_paths(
          graph = graph,
          from = treatments[treatment1_index],
          to = treatments[treatment2_index]
        )
        if (length(paths) != 0) {
          for (path_index in 1:length(paths)) {
            #Only consider loops
            if (length(paths[[path_index]]) > 2) {
              #The number of edges in the loop (equal to the number of treatments in the path)
              n_comparisons <- length(paths[[path_index]])
              supporting_studies <- list()
              #For each comparison except the last, find the supporting studies
              for (comparison_index in 1:(n_comparisons -  1)) {
                supporting_studies[[comparison_index]] <- FindStudiesIncludingTreatments(
                  data = data,
                  treatments = c(
                    names(paths[[path_index]])[comparison_index],
                    names(paths[[path_index]])[comparison_index + 1]
                  ),
                  all_or_any = "all"
                )
              }
              #The last comparison, which has to be done separately as it isn't part of the path
              supporting_studies[[n_comparisons]] <- FindStudiesIncludingTreatments(
                data = data,
                treatments = c(
                  names(paths[[path_index]])[n_comparisons],
                  names(paths[[path_index]])[1]
                ),
                all_or_any = "all"
              )
              #If each edge has a unique set of supporting studies then this loop contains a splittable comparison
              if (length(supporting_studies) == length(unique(supporting_studies))) {
                return(
                  list(
                    is_nodesplittable = TRUE,
                    reason = NULL
                  )
                )
              }
            }
          }
        }
      }
    }
  }
  return(
    list(
      is_nodesplittable = FALSE,
      reason = "In all loops, heterogeneity and inconsistency cannot be distinguished."
    )
  )
}
