
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
  graph <- .CreateGraph(data = data)
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


#' Identify all of the disconnected subnetworks contained in the data.
#'
#' @param data Data frame containing all of the studies for binary or continuous outcomes, and wide or long format.
#' @param treatment_df Data frame containing the names ("Label") and IDs ("Number") of the treatments.
#' @param reference_treatment_name The name of the reference treatment. If not specified, then defaults to treatment with "Number" 1 in treatment_df.
#' @param subnet_name_prefix Names of the subnetworks. Defaults to "subnet_"
#'
#' @return List of subnetworks, where each subnetwork is a list containing:
#' - "treatments" = The IDs of the treatments included in the given network
#' - "studies" = The names of the studies included in the given subnetwork
IdentifySubNetworks_old <- function(data, treatment_df, reference_treatment_name = NULL, subnet_name_prefix = "subnet_") {
  if (is.null(reference_treatment_name)) {
    reference_treatment <- 1
  } else if (!(reference_treatment_name %in% treatment_df$Label)) {
    # Warn users if reference treatment isn't contained within the data
    warning(glue::glue("Reference treatment '{reference_treatment_name}' cannot be found in the data. ",
                       "Using '{treatment_df$Label[1]}' as reference treatment instead"))
    reference_treatment <- 1
  } else {
    reference_treatment <- treatment_df$Number[treatment_df$Label == reference_treatment_name]
  }

  # Check that reference treatment is also in the data, not just in the treatment list
  all_treatments <- FindAllTreatments(data)
  if (!(reference_treatment %in% all_treatments)) {
    new_reference_treatment <- min(all_treatments)
    warning(glue::glue("Reference treatment '{treatment_df$Label[reference_treatment]}' cannot be found in the treatment list. ",
                       "Using '{treatment_df$Label[new_reference_treatment]}' as reference treatment instead"))
    reference_treatment <- new_reference_treatment
  }

  graph <- .CreateGraph(data)
  components <- igraph::components(graph)
  membership <- components$membership

  # Identify sub-networks
  subnet_list <- list()
  reference_found = FALSE
  for (membership_index in unique(membership)) {
    subnet_treatments <- treatment_df$Number[membership == membership_index]
    subnet_studies <- FindStudiesIncludingTreatments(data, subnet_treatments, "any")

    if (length(subnet_studies) == 0) {
      next
    }

    # Subnetwork name and index are defined by the reference treatment.
    # The reference treatment will always be in the first subnetwork
    if (reference_treatment %in% subnet_treatments) {
      subnet_index = 1
      reference_found = TRUE
    } else {
      subnet_index = length(subnet_list) + ifelse(reference_found, 1, 2)
    }
    subnet_name <- paste0(subnet_name_prefix, subnet_index)

    subnet_list[[subnet_name]] <- list(treatments = subnet_treatments, studies = subnet_studies)
  }

  subnet_list <- subnet_list[order(names(subnet_list))]

  return(subnet_list)
}
