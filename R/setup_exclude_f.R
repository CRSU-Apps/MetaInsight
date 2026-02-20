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
