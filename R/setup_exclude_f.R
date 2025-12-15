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
setup_exclude <- function(non_covariate_data, treatment_df, reference_treatment, outcome, outcome_measure, model_type, exclusions, async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("non_covariate_data", "treatment_df", "outcome", "outcome_measure", "reference_treatment", "model_type"),
                            c("data.frame", "data.frame", "character", "character", "character", "character"), NULL)){
      return()
    }
  }

  if (!is.null(exclusions) && !inherits(exclusions, "character")){
    return(async |> asyncLog(type = "error", "error", "exclusions must be of class character"))
  }

  if (!outcome %in% c("Binary", "Continuous")){
    return(async |> asyncLog(type = "error", "error", "outcome must be either Binary or Continuous"))
  }

  if (outcome == "Binary" && !outcome_measure %in% c("OR", "RR", "RD")){
    return(async |> asyncLog(type = "error", "When outcome is Binary, outcome_measure must be either OR, RR or RD"))
  }

  if (outcome == "Continuous" && !outcome_measure %in% c("MD", "SMD")){
    return(async |> asyncLog(type = "error", "When outcome is Continuous, outcome_measure must be either MD or SMD"))
  }

  if (!model_type %in% c("random", "fixed")){
    return(async |> asyncLog(type = "error", "model_type must be either random or fixed"))
  }

  subsetted_data <- non_covariate_data[!non_covariate_data$Study %in% exclusions,]

  if (nrow(subsetted_data) == 0){
    return(async |> asyncLog(type = "error", "You have excluded all the studies"))
  }

  dewrangled_data_sub <- ReinstateTreatmentIds(subsetted_data, treatment_df)
  subsetted_treatment_df <- FindAllTreatments(dewrangled_data_sub)

  treatment_df_sub <- CreateTreatmentIds(subsetted_treatment_df, reference_treatment)
  data_sub <- ReplaceTreatmentIds(dewrangled_data_sub, treatment_df_sub)
  non_covariate_data_sub <- RemoveCovariates(data_sub)

  reference_treatment_sub <- treatment_df_sub$Label[treatment_df_sub$Number == 1]

  bugsnet_sub <- bugsnetdata(non_covariate_data_sub,
                             outcome,
                             treatment_df_sub)

  freq_sub <- frequentist(non_covariate_data_sub,
                          outcome,
                          treatment_df_sub,
                          outcome_measure,
                          model_type,
                          reference_treatment_sub)

  list(bugsnet_sub = bugsnet_sub,
       freq_sub = freq_sub,
       reference_treatment_sub = reference_treatment_sub,
       subsetted_data = data_sub,
       subsetted_treatment_df = treatment_df_sub
       )
}
