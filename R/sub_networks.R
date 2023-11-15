#' 
#' #' Identify all of the disconnected subnetworks contained in the data.
#' #'
#' #' @param data Data containing all of the studies for binary or continuous outcomes, and wide or long format.
#' #' @param reference_treatment ID of the reference treatment if applicable. Defaults to 1.
#' #' @param subnet_name_prefix Names of the subnetworks. Defaults to "subnet_"
#' IdentifySubNetworks <- function(data, treatment_df, reference_treatment = NULL, subnet_name_prefix = "subnet_") {
#'   # Warn users if reference treatment isn't contained within the data
#'   if (!is.null(reference_treatment) && !(reference_treatment %in% unique(treatment_df$Label))) {
#'     warning(glue::glue("Reference treatment '{reference_treatment}' cannot be found in the data. Using {treatment_df$Label[1]} as reference treatment instead"))
#'     reference_treatment = treatment_df$Label[1]
#'   } else if (is.null(reference_treatment)) {
#'     reference_treatment = treatment_df$Label[1]
#'   }
#'   
#'   # Find links
#'   links = c()
#'   for (study in unique(data$Study)) {
#'     study_treatments <- FindAllTreatments(data[data$Study == study, ])
#'     for (i in 1:(length(study_treatments) - 1)) {
#'       for (j in (i + 1):length(study_treatments)) {
#'         links <- c(links, study_treatments[i], study_treatments[j])
#'       }
#'     }
#'   }
#'   
#'   # Build network (Mathematical structure is called a 'graph')
#'   graph <- igraph::graph(links, directed = FALSE)
#'   components <- igraph::components(graph)
#'   membership <- components$membership
#'   
#'   # Identify sub-networks
#'   subnet_list <- list()
#'   reference_found = FALSE
#'   for (membership_index in unique(membership)) {
#'     subnet_treatments <- treatment_df$Label[membership == membership_index]
#'     subnet_studies <- FindStudiesIncludingTreatments(data, subnet_treatments)
#'     
#'     # Subnetwork name and index are defined by the reference treatment.
#'     # The reference treatment will always be in the first subnetwork
#'     if (reference_treatment %in% subnet_treatments) {
#'       subnet_index = 1
#'       reference_found = TRUE
#'     } else {
#'       subnet_index = length(subnet_list) + ifelse(reference_found, 1, 2)
#'     }
#'     subnet_name <- paste0(subnet_name_prefix, subnet_index)
#'     
#'     subnet_list[[subnet_name]] <- list(treatments = subnet_treatments, studies = subnet_studies)
#'   }
#'   
#'   subnet_list <- subnet_list[order(names(subnet_list))]
#'   
#'   return(subnet_list)
#' }


#' Create a graph where the nodes are treatments and the links are studies comparing those treatments.
#'
#' @param data Data for which to create graph.
#'
#' @return Created igraph object.
.CreateGraph <- function(data) {
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
  return(igraph::graph(links, directed = FALSE))
}

#' Identify all of the disconnected subnetworks contained in the data.
#'
#' @param data Data containing all of the studies for binary or continuous outcomes, and wide or long format.
#' @param reference_treatment ID of the reference treatment if applicable. Defaults to 1.
#' @param subnet_name_prefix Names of the subnetworks. Defaults to "subnet_"
IdentifySubNetworks <- function(data, treatment_df, reference_treatment = 1, subnet_name_prefix = "subnet_") {
  # Warn users if reference treatment isn't contained within the data
  if (reference_treatment != 1 & !(reference_treatment %in% unique(treatment_df$Number))) {
    warning(paste0("Reference treatment '", reference_treatment, "' cannot be found in the data. ",
                   "Using '1' as reference treatment instead"))
    reference_treatment = 1
  }

  graph <- .CreateGraph(data)
  components <- igraph::components(graph)
  membership <- components$membership

  # Identify sub-networks
  subnet_list <- list()
  reference_found = FALSE
  for (membership_index in unique(membership)) {
    subnet_treatments <- treatment_df$Number[membership == membership_index]
    subnet_studies <- FindStudiesIncludingTreatments(data, subnet_treatments)

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
