load_define <- function(data, treatment_df, reference_treatment){

  main_subnetworks <- IdentifySubNetworks(data, treatment_df, reference_treatment)

  indices <- seq_along(data$Study)

  primary_network <- main_subnetworks$subnet_1

  connected_indices <- indices[data$Study %in% primary_network$studies]

  main_connected_data <- data[connected_indices,]

  studies <- unique(data$Study)
  main_subnetwork_exclusions <- studies[!studies %in% main_connected_data$Study]

}


#' Find the expected reference treatment from a vector.
#' This is done by comparing treatment names to expected reference treatment names.
#'
#' @param treatments vector containing all treatment names
#' @return Name of the expected reference treatment if one is found, else NULL
#' @export
FindExpectedReferenceTreatment <- function(treatments) {
  expected_reference_treatments <- match(.potential_reference_treatments, tolower(treatments))
  expected_reference_treatments <- expected_reference_treatments[!is.na(expected_reference_treatments)]
  if (length(expected_reference_treatments) > 0) {
    return(treatments[expected_reference_treatments[1]])
  } else {
    return(NULL)
  }
}

#' Returns the default ranking order, for the example datasets.
#'
#' @param outcome "Binary" or "Continuous".
#' @param data Input dataset.
#' @return "good" or "bad".
#' @export
RankingOrder <- function(outcome, data) {
  file1 <- data
  if (outcome == "Binary" & is.null(file1)) {
    choice <- "bad"
  } else {
    choice <- "good"
  }
  return(choice)
}
