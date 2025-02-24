#' Checks the connectivity of the uploaded data and converts it into formats for
#' bugsnet and frequentist analyses
#'
#' @param data dataframe. Uploaded data
#' @param treatment_df dataframe. Treatments
#' @param outcome character. Outcome type for the dataset. Either `Binary` or
#' `Continuous`.
#' @param outcome_measure character. Outcome measure of the dataset. Either
#' `OR`, `RR` or `RD` when `outcome` is `Binary` or `MD` or `SMD` when
#' `outcome` is `Continuous`
#' @param reference_treatment character. The reference treatment of the dataset
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return List containing:
#'  \item{wrangled_data}{dataframe. To be presented in the data table}
#'  \item{treatment_df}{dataframe. Updated version of the input parameter}
#'  \item{disconnected_indices}{vector. Indices of studies that are not connected to the main network}
#'  \item{main_connected_data}{dataframe. A subset of the data containing only connected studies}
#'  \item{non_covariate_data_all}{dataframe. The uploaded data with covariates removed}
#'  \item{bugsnet_all}{dataframe. Processed data for bugsnet analyses created by `bugsnetdata`}
#'  \item{freq_all}{list. Processed data for frequentist analyses created by `frequentist()`}
#' @export
#'

setup_define <- function(data, treatment_df, outcome, outcome_measure, reference_treatment, logger = NULL){

  check_param_classes(c("data", "treatment_df", "outcome", "outcome_measure", "reference_treatment"),
                      c("data.frame", "data.frame", "character", "character", "character"), logger)

  if (!outcome %in% c("Binary", "Continuous")){
    logger %>% writeLog(type = "error", "outcome must be either Binary or Continuous")
    return()
  }

  if (outcome == "Binary" && !outcome_measure %in% c("OR", "RR", "RD")){
    logger %>% writeLog(type = "error", "When outcome is Binary, outcome_measure must be either OR, RR or RD")
    return()
  }

  if (outcome == "Continuous" && !outcome_measure %in% c("MD", "SMD")){
    logger %>% writeLog(type = "error", "When outcome is Continuous, outcome_measure must be either MD or SMD")
    return()
  }

  # update using the selected reference treatment
  treatment_df <- CreateTreatmentIds(treatment_df$Label, reference_treatment)

  data <- WrangleUploadData(data, treatment_df, outcome)

  main_subnetworks <- IdentifySubNetworks(data, treatment_df, reference_treatment)

  indices <- seq_along(data$Study)

  primary_network <- main_subnetworks$subnet_1

  connected_indices <- indices[data$Study %in% primary_network$studies]

  main_connected_data <- data[connected_indices,]

  studies <- unique(data$Study)
  main_subnetwork_exclusions <- studies[!studies %in% main_connected_data$Study]

  if (length(main_subnetwork_exclusions) > 0){
    logger %>% writeLog(type = "warning",
      glue::glue("The uploaded data comprises a disconnected network.
                 Only the subnetwork containing the reference treatment
                 ({reference_treatment}) will be displayed and disconnected
                 studies are shown in the logger."))

    logger %>% writeLog(paste0("Disconnected studies: ", paste(main_subnetwork_exclusions, collapse = ",")))

  }

  disconnected_indices <- which(studies %in% main_subnetwork_exclusions)

  non_covariate_data_all <- RemoveCovariates(main_connected_data)

  bugsnet_all <- bugsnetdata(non_covariate_data_all, outcome, treatment_df)

  # random is the default model type, this structure is updated in summary_exclude if the model type changes
  # suppressWarnings deprecation temporarily
  freq_all <- suppressWarnings(frequentist(non_covariate_data_all,
                          outcome,
                          treatment_df,
                          outcome_measure,
                          "random",
                          treatment_df$Label[treatment_df$Number == 1]))

  return(list(wrangled_data = data,
              treatment_df = treatment_df,
              disconnected_indices = disconnected_indices,
              main_connected_data = main_connected_data,
              non_covariate_data_all = non_covariate_data_all,
              bugsnet_all = bugsnet_all,
              freq_all = freq_all))
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

  graph <- .CreateGraph(data)
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
