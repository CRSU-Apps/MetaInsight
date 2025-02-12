common_class <- R6::R6Class(
  classname = "common",
  public = list(
   data = NULL,
   upgraded_data = NULL,
   wrangled_data = NULL,
   is_data_valid = NULL,
   is_data_uploaded = NULL,
   outcome = NULL,
   treatment_df = NULL,
   outcome_measure = NULL,
   reference_treatment = NULL,
   disconnected_indices = NULL,
   main_connected_data = NULL,
   non_covariate_data_all = NULL,
   model_type = NULL,
   excluded_studies = NULL,
   sensitivity_data = NULL,
   sensitivity_treatment_df = NULL,
   bugsnet_all = NULL,
   bugsnet_sub = NULL,
   freq_all = NULL,
   freq_sub = NULL,
   meta = NULL,
   logger = NULL,
   state = NULL,
   reset = function(){
     self$data <- NULL
     self$upgraded_data <- NULL
     self$wrangled_data <- NULL
     self$valid_data <- NULL
     self$is_data_uploaded <- NULL
     self$outcome <- NULL
     self$treatment_df <- NULL
     self$outcome_measure <- NULL
     self$reference_treatment <- NULL
     self$disconnected_indices <- NULL
     self$main_connected_data <- NULL
     self$non_covariate_data_all <- NULL
     self$model_type <- NULL
     self$excluded_studies <- NULL
     self$sensitivity_data <- NULL
     self$sensitivity_treatment_df <- NULL
     self$bugsnet_all <- NULL
     self$bugsnet_sub <- NULL
     self$freq_all <- NULL
     self$freq_sub <- NULL
     self$meta <- NULL
     self$state <- NULL
     invisible(self)
   }
  )
)

