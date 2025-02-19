
#' Create a graph where the nodes are treatments and the links are studies comparing those treatments.
#'
#' @param data Data for which to create graph.
#' @param exclude_link Vector containing the numbers of two treatments for which links will not be added. Defaults to NULL.
#'
#' @return Created igraph object.
CreateGraph <- function(data, exclude_link = NULL) {
  # Find links
  links = c()
  for (study in unique(data$Study)) {
    study_treatments <- FindAllTreatments(data[data$Study == study, ])
    for (i in 1:(length(study_treatments) - 1)) {
      for (j in (i + 1):length(study_treatments)) {
        if (!setequal(c(study_treatments[i], study_treatments[j]), exclude_link)) {
          links <- c(links, study_treatments[i], study_treatments[j])
        }
      }
    }
  }
  
  # Build network (Mathematical structure is called a 'graph')
  return(igraph::make_graph(links, directed = FALSE))
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
IdentifySubNetworks <- function(data, treatment_df, reference_treatment_name = NULL, subnet_name_prefix = "subnet_") {
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

  graph <- CreateGraph(data)
  components <- igraph::components(graph)
  membership <- components$membership

  # Identify sub-networks
  subnet_list <- list()
  reference_found = FALSE
  for (membership_index in unique(membership)) {
    subnet_treatments <- treatment_df$Number[membership == membership_index]
    subnet_studies <- FindStudiesIncludingTreatments(data, subnet_treatments)
    
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
