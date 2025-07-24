# The aim here is to process the data once at the start so it can be used downstream

loaded_data_con <- setup_load(outcome = "Continuous")
defined_data_con <- setup_configure(loaded_data_con$data, loaded_data_con$treatment_df, "Continuous", "MD", "Placebo")
excluded_data_con <- setup_exclude(defined_data_con$non_covariate_data_all, defined_data_con$treatment_df, "Placebo", "Continuous", "MD", "random", c("Study01", "Study02", "Study03", "Study04"))
loaded_data_bin <- setup_load(outcome = "Binary")
defined_data_bin <- setup_configure(loaded_data_bin$data, loaded_data_bin$treatment_df, "Binary", "OR", "Placebo")

fitted_bayes_model <- bayes_model(defined_data_con$main_connected_data, loaded_data_con$treatment_df, "Continuous", "MD", "random", "Placebo")

test_data_dir <- normalizePath(testthat::test_path("data"))

# fit a Bayesian model and save it so it can be reloaded in other tests
bayes_model_path <- "~/temprds/bayes.rds" #tempfile(fileext = ".rds")


app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
app$click("setup_load-run")
app$click("setup_configure-run")
app$set_inputs("setup_exclude-exclusions" = "Leo")
app$wait_for_value(input = "setup_exclude-complete")
app$click("bayes_model-run")
app$wait_for_value(input = "bayes_model-all-complete")
app$wait_for_value(input = "bayes_model-sub-complete")
app$click("save-button")
app$get_download("core_save-save_session", filename = bayes_model_path)
app$stop()
