#' Assess the data for validity. this checks the column names for required columns, and balanced wide format numbered columns.
#'
#' @param data dataframe. Uploaded data
#' @param treatment_df vector of treatments
#' @param reference_treatment character. The reference treatment
#' @return list
#' @export
summary_exclude <- function(data, treatment_df, reference_treatment, outcome, outcome_measure, model_type, exclusions, logger = NULL){

  selected_data <- data[!data$Study %in% exclusions,]

  # I find this baffling - why not just add the IDs once when data is loaded?
  sensitivity_dewrangled_data <- ReinstateTreatmentIds(selected_data, treatment_df)
  sensitivity_treatments <- FindAllTreatments(sensitivity_dewrangled_data)
  sensitivity_treatments <- unique(sensitivity_dewrangled_data$T)

  sensitivity_treatment_df <- CreateTreatmentIds(sensitivity_treatments, reference_treatment)
  sensitivity_data <- ReplaceTreatmentIds(sensitivity_dewrangled_data, sensitivity_treatment_df)
  sensitivity_non_covariate_data <- RemoveCovariates(sensitivity_data)

  bugsnet_sub <- bugsnetdata(sensitivity_non_covariate_data,
                               outcome,
                               sensitivity_treatment_df)

  freq_sub <- frequentist(sensitivity_non_covariate_data,
                          outcome,
                          sensitivity_treatment_df,
                          outcome_measure,
                          model_type,
                          sensitivity_treatment_df$Label[sensitivity_treatment_df$Number == 1]) #this should be handled inside the function!

  list(bugsnet_sub = bugsnet_sub,
       freq_sub = freq_sub)
}
