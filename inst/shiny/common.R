common_class <- R6::R6Class(
  classname = "common",
  public = list(
   data = NULL,
   upgraded_data = NULL,
   is_data_valid = NULL,
   is_data_uploaded = NULL,
   metaoutcome = NULL,
   treatment_df = NULL,
   outcome_measure = NULL,
   reference_treatment = NULL,
   disconnected_indices = NULL,
   main_connected_data = NULL,
   initial_non_covariate_data = NULL,
   model_type = NULL,
   excluded_studies = NULL,
   sensitivity_data = NULL,
   sensitivity_treatment_df = NULL,
   bugsnetdt = NULL,
   bugsnetdt_sub = NULL,
   freq_all = NULL,
   freq_sub = NULL,
   meta = NULL,
   logger = NULL,
   state = NULL,
   reset = function(){
     self$data <- NULL
     self$default_data <- NULL
     self$upgraded_data <- NULL
     self$valid_data <- NULL
     self$is_data_uploaded <- NULL
     self$treatment_list <- NULL
     self$metaoutcome <- NULL
     self$treatment_df <- NULL
     self$outcome_measure <- NULL
     self$reference_treatment <- NULL
     self$initial_data <- NULL
     self$sensitivity_data <- NULL
     self$sensitivity_treatment_list <- NULL
     self$bugsnetdt <- NULL
     self$bugsnetdt_sub <- NULL
     self$freq_all <- NULL
     self$freq_sub <- NULL
     self$meta <- NULL
     self$state <- NULL
     invisible(self)
   }
  )
)

