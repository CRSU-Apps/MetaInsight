  common_class <- R6::R6Class(
    classname = "common",
    public = list(
  continuous_file = NULL,
 binary_file = NULL,
 data = NULL,
 is_data_default = NULL,
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
 state = NULL)
  )

