common_class <- R6::R6Class(
  classname = "common",
  public = list(
   data = NULL,
   default_data = NULL,
   upgraded_data = NULL,
   valid_data = NULL,
   is_data_uploaded = NULL,
   treatment_list = NULL,
   metaoutcome = NULL,
   treatment_df = NULL,
   outcome_measure = NULL,
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

