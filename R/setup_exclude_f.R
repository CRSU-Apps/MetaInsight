#' Takes the uploaded data, removes any excluded studies and returns
#' subsets of the data in formats for bugsnet and frequentist analyses
#'
#' @param non_covariate_data dataframe. Data processed by setup_configure()
#' @param treatment_df dataframe. Treatments and their IDs
#' @param reference_treatment character. The reference treatment of the dataset
#' @param outcome character. Outcome type for the dataset. Either `Binary` or
#' `Continuous`.
#' @param outcome_measure character. Outcome measure of the dataset. Either
#' `OR`, `RR` or `RD` when `outcome` is `Binary` or `MD` or `SMD` when
#' `outcome` is `Continuous`
#' @param model_type character. Type of model to fit, either `random` or `fixed`
#' @param exclusions character. Vector of study names to exclude.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return List containing:
#'  \item{bugsnet_sub}{dataframe. Processed data for bugsnet analyses created by `bugsnetdata()`}
#'  \item{freq_sub}{list. Processed data for frequentist analyses created by `frequentist()`}
#'  \item{reference_treatment_sub}{character. Reference treatment for sensitivity analyses}
#'  \item{subsetted_data}{dataframe. Processed data}
#'  \item{subsetted_treatment_df}{dataframe. Treatments in processed data}
#'
#' @export
#'
setup_exclude <- function(non_covariate_data, treatment_df, reference_treatment, outcome, outcome_measure, model_type, exclusions, logger = NULL){

  check_param_classes(c("non_covariate_data", "treatment_df", "outcome", "outcome_measure", "reference_treatment", "model_type"),
                      c("data.frame", "data.frame", "character", "character", "character", "character"), logger)

  if (!is.null(exclusions) && !inherits(exclusions, "character")){
    logger |> writeLog(type = "error", "exclusions must be of class character")
    return()
  }

  if (!outcome %in% c("Binary", "Continuous")){
    logger |> writeLog(type = "error", "outcome must be either Binary or Continuous")
    return()
  }

  if (outcome == "Binary" && !outcome_measure %in% c("OR", "RR", "RD")){
    logger |> writeLog(type = "error", "When outcome is Binary, outcome_measure must be either OR, RR or RD")
    return()
  }

  if (outcome == "Continuous" && !outcome_measure %in% c("MD", "SMD")){
    logger |> writeLog(type = "error", "When outcome is Continuous, outcome_measure must be either MD or SMD")
    return()
  }

  if (!model_type %in% c("random", "fixed")){
    logger |> writeLog(type = "error", "model_type must be either random or fixed")
    return()
  }

  subsetted_data <- non_covariate_data[!non_covariate_data$Study %in% exclusions,]
  dewrangled_data_sub <- ReinstateTreatmentIds(subsetted_data, treatment_df)
  subsetted_treatment_df <- FindAllTreatments(dewrangled_data_sub)

  treatment_df_sub <- CreateTreatmentIds(subsetted_treatment_df, reference_treatment)
  data_sub <- ReplaceTreatmentIds(dewrangled_data_sub, treatment_df_sub)
  non_covariate_data_sub <- RemoveCovariates(data_sub)

  reference_treatment_sub <- treatment_df_sub$Label[treatment_df_sub$Number == 1]

  bugsnet_sub <- bugsnetdata(non_covariate_data_sub,
                             outcome,
                             treatment_df_sub)

  freq_sub <- suppressWarnings(
                frequentist(non_covariate_data_sub,
                          outcome,
                          treatment_df_sub,
                          outcome_measure,
                          model_type,
                          reference_treatment_sub)
              )

  list(bugsnet_sub = bugsnet_sub,
       freq_sub = freq_sub,
       reference_treatment_sub = reference_treatment_sub,
       subsetted_data = data_sub,
       subsetted_treatment_df = treatment_df_sub
       )
}
