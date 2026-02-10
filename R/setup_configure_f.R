#' Checks the connectivity of the uploaded data and converts it into formats for
#' bugsnet and frequentist analyses
#'
#' @param data dataframe. Uploaded data
#' @inheritParams common_params
#' @return List containing:
#'  \item{wrangled_data}{dataframe. To be presented in the data table}
#'  \item{treatments}{dataframe. Treatment names and IDs}
#'  \item{reference_treatment}{character. The selected reference treatment}
#'  \item{disconnected_indices}{vector. Indices of studies that are not connected to the main network}
#'  \item{connected_data}{dataframe. A subset of the data containing only connected studies}
#'  \item{non_covariate_data}{dataframe. The uploaded data with covariates removed}
#'  \item{covariate_column}{character. Name of the column containing covariate data}
#'  \item{covariate_name}{character. Name of the covariate}
#'  \item{covariate_type}{character. Whether the covariate is `binary` or `continuous`}
#'  \item{bugsnet}{dataframe. Processed data for bugsnet analyses created by `bugsnetdata`}
#'  \item{freq}{list. Processed data for frequentist analyses created by `frequentist()`}
#'  \item{outcome}{character. Whether the data is `binary` or `continuous`}
#'  \item{outcome_measure}{character. Outcome measure of the dataset.}
#'  \item{effects}{character. Whether the models are `fixed` or `random` effects}
#'  \item{ranking_option}{character. Whether higher values in the data are `good` or `bad`}
#'  \item{seed}{numeric. A seed value to be passed to models}
#' @export
#'

setup_configure <- function(loaded_data, reference_treatment, effects, outcome_measure, ranking_option, seed, logger = NULL){

  check_param_classes(c("loaded_data", "reference_treatment", "effects", "outcome_measure", "ranking_option", "seed"),
                      c("loaded_data", "character", "character", "character", "character", "numeric"), logger)

  if (!reference_treatment %in% loaded_data$treatments$Label){
    logger |> writeLog(type = "error", "reference_treatment must be present in the loaded data")
    return()
  }

  if (!effects %in% c("random", "fixed")){
    logger |> writeLog(type = "error", "effects must be either random or fixed")
    return()
  }

  if (loaded_data$outcome == "binary" && !outcome_measure %in% c("OR", "RR", "RD")){
    logger |> writeLog(type = "error", "When outcome is binary, outcome_measure must be either OR, RR or RD")
    return()
  }

  if (loaded_data$outcome == "continuous" && !outcome_measure %in% c("MD", "SMD")){
    logger |> writeLog(type = "error", "When outcome is continuous, outcome_measure must be either MD or SMD")
    return()
  }

  if (!ranking_option %in% c("good", "bad")){
    logger |> writeLog(type = "error", "ranking_option must be either good or bad")
    return()
  }

  # update using the selected reference treatment
  treatments <- CreateTreatmentIds(loaded_data$treatments$Label, reference_treatment)

  data <- WrangleUploadData(loaded_data$data, treatments, loaded_data$outcome)
  treatments <- CleanTreatmentIds(treatments)
  reference_treatment <- CleanStrings(reference_treatment)

  subnetworks <- IdentifySubNetworks(data, treatments, reference_treatment)

  indices <- seq_along(data$Study)

  primary_network <- subnetworks$subnet_1

  connected_indices <- indices[data$Study %in% primary_network$studies]

  connected_data <- data[connected_indices,]

  studies <- unique(data$Study)
  subnetwork_exclusions <- studies[!studies %in% connected_data$Study]

  if (length(subnetwork_exclusions) > 0){
    logger |> writeLog(type = "warning",
      glue::glue("The uploaded data comprises a disconnected network.
                 Only the subnetwork containing the reference treatment
                 ({reference_treatment}) will be displayed and disconnected
                 studies are shown in the logger."))

    logger |> writeLog(paste0("Disconnected studies: ", paste(subnetwork_exclusions, collapse = ",")))

  }

  disconnected_indices <- which(studies %in% subnetwork_exclusions)

  non_covariate_data <- RemoveCovariates(connected_data)

  if (any(grepl("covar\\.", names(connected_data)))){
    covariate_column <- FindCovariateNames(connected_data)
    covariate <- list(
      column = covariate_column,
      name = GetFriendlyCovariateName(covariate_column),
      type = InferCovariateType(connected_data[[covariate_column]])
    )
  } else {
    covariate <- list()
  }

  bugsnet <- bugsnetdata(non_covariate_data, loaded_data$outcome, treatments)

  # random is the default model type, this structure is updated in setup_exclude if the model type changes
  freq <- frequentist(non_covariate_data,
                      loaded_data$outcome,
                      treatments,
                      outcome_measure,
                      effects,
                      treatments$Label[treatments$Number == 1])

  output <- list(wrangled_data = data,
                 treatments = treatments,
                 reference_treatment = reference_treatment,
                 disconnected_indices = disconnected_indices,
                 connected_data = connected_data,
                 non_covariate_data = non_covariate_data,
                 covariate = covariate,
                 bugsnet = bugsnet,
                 freq = freq,
                 outcome = loaded_data$outcome,
                 outcome_measure = outcome_measure,
                 effects = effects,
                 ranking_option = ranking_option,
                 seed = seed)

  class(output) <- "configured_data"

  return(output)
}

#' Identify all of the disconnected subnetworks contained in the data.
#'
#' @param data Data frame containing all of the studies for binary or continuous outcomes, and wide or long format.
#' @param treatments Data frame containing the names ("Label") and IDs ("Number") of the treatments.
#' @param reference_treatment_name The name of the reference treatment. If not specified, then defaults to treatment with "Number" 1 in treatments.
#' @param subnet_name_prefix Names of the subnetworks. Defaults to "subnet_"
#'
#' @return List of subnetworks, where each subnetwork is a list containing:
#' - "treatments" = The IDs of the treatments included in the given network
#' - "studies" = The names of the studies included in the given subnetwork
IdentifySubNetworks <- function(data, treatments, reference_treatment_name = NULL, subnet_name_prefix = "subnet_") {
  if (is.null(reference_treatment_name)) {
    reference_treatment <- 1
  } else if (!(reference_treatment_name %in% treatments$Label)) {
    # Warn users if reference treatment isn't contained within the data
    warning(glue::glue("Reference treatment '{reference_treatment_name}' cannot be found in the data. ",
                       "Using '{treatments$Label[1]}' as reference treatment instead"))
    reference_treatment <- 1
  } else {
    reference_treatment <- treatments$Number[treatments$Label == reference_treatment_name]
  }

  # Check that reference treatment is also in the data, not just in the treatment list
  all_treatments <- FindAllTreatments(data)
  if (!(reference_treatment %in% all_treatments)) {
    new_reference_treatment <- min(all_treatments)
    warning(glue::glue("Reference treatment '{treatments$Label[reference_treatment]}' cannot be found in the treatment list. ",
                       "Using '{treatments$Label[new_reference_treatment]}' as reference treatment instead"))
    reference_treatment <- new_reference_treatment
  }

  graph <- CreateGraph(data)
  components <- igraph::components(graph)
  membership <- components$membership

  # Identify sub-networks
  subnet_list <- list()
  reference_found <- FALSE
  for (membership_index in unique(membership)) {
    subnet_treatments <- treatments$Number[membership == membership_index]
    subnet_studies <- FindStudiesIncludingTreatments(data, subnet_treatments, "any")

    if (length(subnet_studies) == 0) {
      next
    }

    # Subnetwork name and index are defined by the reference treatment.
    # The reference treatment will always be in the first subnetwork
    if (reference_treatment %in% subnet_treatments) {
      subnet_index <- 1
      reference_found <- TRUE
    } else {
      subnet_index <- length(subnet_list) + ifelse(reference_found, 1, 2)
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
#' @param outcome "binary" or "continuous".
#' @param data Input dataset.
#' @return "good" or "bad".
#' @export
RankingOrder <- function(outcome, data) {
  file1 <- data
  if (outcome == "binary" & is.null(file1)) {
    choice <- "bad"
  } else {
    choice <- "good"
  }
  return(choice)
}

#' Create a copy of a data from which does not contain any covariate columns.
#'
#' @param data Data from which to remove covariate columns
#' @return Data without covariate columns
#' @export
RemoveCovariates <- function(data) {
  covariate_column_names <- FindCovariateNames(data)

  if (length(covariate_column_names) == 0) {
    return(data)
  }

  covariate_column_indices <- match(covariate_column_names, names(data))
  return(data[, -covariate_column_indices])
}


InferCovariateType <- function(covariate_data) {
  unique_items <- unique(covariate_data)
  if (length(unique_items) == 2 && all(sort(unique_items) == c(0, 1))) {
    return("binary")
  } else {
    return("continuous")
  }
}

#' Converts wide data to long, adds the variable 'se' for a continuous outcomes, and does some formatting.
#'
#' @param newData1 Input dataset.
#' @param treat_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param CONBI "continuous" or "binary".
#' @return Input data set in long format with the variable 'se' for a continuous outcome.
dataform.df <- function(newData1, treat_list, CONBI) {
  if (FindDataShape(newData1) == "long") {
    long <- newData1
  } else {
    long <- WideToLong(wide_data = newData1, outcome = CONBI)
  }

  long_sort <- long[order(long$StudyID, -long$T), ]
  if (CONBI == 'continuous') {
    long_sort$se <- long_sort$SD / sqrt(long_sort$N)
  }

  long_sort <- ReinstateTreatmentIds(data = long_sort, treatment_ids = treat_list)

  return(long_sort)
}

#' Reformats the treatment labels then calls dataform.df().
#'
#' @param data Input dataset.
#' @param metaoutcome "continuous" or "binary".
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @return Output from dataform.df().
bugsnetdata <- function(data, metaoutcome, treatment_list){
  newData1 <- as.data.frame(data)
  treatment_list$Label <- stringr::str_wrap(gsub("_", " ", treatment_list$Label), width = 10)  # better formatting (although does assume underscores have only been added due to the treatment label entry limitations) CRN
  longsort2 <- dataform.df(newData1 = newData1,
                           treat_list = treatment_list,
                           CONBI = metaoutcome)
  return(longsort2)
}
