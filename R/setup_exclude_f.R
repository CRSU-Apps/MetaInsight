#' Takes the uploaded data, removes any excluded studies and returns
#' subsets of the data in formats for bugsnet and frequentist analyses
#'
#' @param non_covariate_data dataframe. Data processed by setup_configure()
#' @param exclusions character. Vector of study names to exclude.
#' @inheritParams common_params
#' @return List containing:
#'  \item{bugsnet_sub}{dataframe. Processed data for bugsnet analyses created by `bugsnetdata()`}
#'  \item{freq_sub}{list. Processed data for frequentist analyses created by `frequentist()`}
#'  \item{reference_treatment_sub}{character. Reference treatment for sensitivity analyses}
#'  \item{subsetted_data}{dataframe. Processed data}
#'  \item{subsetted_treatment_df}{dataframe. Treatments in processed data}
#'
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

  output
}
