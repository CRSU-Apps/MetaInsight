# The aim here is to process the data once at the start so it can be used downstream

loaded_data_con <- setup_load(outcome = "Continuous")
defined_data_con <- setup_define(loaded_data_con$data, loaded_data_con$treatment_df, "Continuous", "MD", "Placebo")
excluded_data_con <- summary_exclude(defined_data_con$non_covariate_data_all, defined_data_con$treatment_df, "Placebo", "Continuous", "MD", "random", c("Study01", "Study25"))
loaded_data_bin <- setup_load(outcome = "Binary")
defined_data_bin <- setup_define(loaded_data_bin$data, loaded_data_bin$treatment_df, "Binary", "OR", "Placebo")

