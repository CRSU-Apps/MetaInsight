
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

#' Identify which studies can be used to compare each treatment to the reference.
#'
#' @param data Data frame containing all of the studies for binary or continuous outcomes, and wide or long format.
#' @param treatment_df Data frame containing the names ("Label") and IDs ("Number") of the treatments.
#' @param reference_treatment_name The name of the reference treatment. If not specified, then defaults to treatment with "Number" 1 in treatment_df.
#'
#' @return Boolean matrix where rows are studies, columns are treatments, and contents are whether the study compares the treatment to the reference, either directly or indirectly.
FindAllStudiesBetweenTreatments <- function(data, treatment_df, reference_treatment_name = NULL) {
  if (is.null(reference_treatment_name)) {
    reference_treatment <- 1
  } else if (!(reference_treatment_name %in% treatment_df$Label)) {
    # Warn users if reference treatment isn't contained within the data
    warning(
      glue::glue(
        "Reference treatment '{reference_treatment_name}' cannot be found in the data. ",
        "Using '{treatment_df$Label[1]}' as reference treatment instead"
      )
    )
    reference_treatment <- 1
  } else {
    reference_treatment <- treatment_df$Number[treatment_df$Label == reference_treatment_name]
  }
  
  # Check that reference treatment is also in the data, not just in the treatment list
  all_treatments <- FindAllTreatments(data)
  if (!(reference_treatment %in% all_treatments)) {
    new_reference_treatment <- min(all_treatments)
    warning(
      glue::glue(
        "Reference treatment '{treatment_df$Label[reference_treatment]}' cannot be found in the treatment list. ",
        "Using '{treatment_df$Label[new_reference_treatment]}' as reference treatment instead"
      )
    )
    reference_treatment <- new_reference_treatment
  }
  
  all_studies <- unique(data$Study)
  non_reference_treatments <- treatment_df$Label[-reference_treatment]
  
  connected <- matrix(
    data = FALSE,
    nrow = length(all_studies),
    ncol = length(non_reference_treatments)
  )
  row.names(connected) <- all_studies
  colnames(connected) <- non_reference_treatments
  
  study_treatment_ids <- sapply(
    all_studies,
    function(study) {
      FindAllTreatments(data = data, study = study)
    }
  )
  
  # Turn list into matrix
  # This is only needed when there are different numbers of treatment arms between studies
  if (is.list(study_treatment_ids)) {
    max_treatments <- max(
      sapply(
        names(study_treatment_ids),
        function(name) {
          length(study_treatment_ids[[name]])
        }
      )
    )
    
    temp_matrix <- matrix(
      nrow = max_treatments,
      ncol = length(all_studies)
    )
    colnames(temp_matrix) <- all_studies
    
    sapply(
      all_studies,
      function(study) {
        treatments <- study_treatment_ids[[study]]
        temp_matrix[1:length(treatments), study] <<- treatments
      }
    )
    
    study_treatment_ids <- temp_matrix
  }
  
  graph <- .CreateGraph(data)
  
  for (treatment_id in treatment_df$Number[-reference_treatment]) {
    paths <- igraph::all_simple_paths(graph = graph, from = reference_treatment, to = treatment_id)
    
    for (path in paths) {
      for (index in 1:(length(path) - 1)) {
        from <- path[index]
        to <- path[index + 1]
        
        edge_studies <- unlist(
          lapply(
            all_studies,
            function(study) {
              treatments <- study_treatment_ids[, study]
              if (from %in% treatments && to %in% treatments) {
                return(study)
              }
            }
          )
        )
        
        treatment_name <- treatment_df$Label[treatment_df$Number == treatment_id]
        connected[edge_studies, treatment_name] <- TRUE
      }
    }
  }
  
  return(connected)
}