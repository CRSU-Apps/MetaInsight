# The aim here is to process the data once at the start so it can be used downstream
test_data_dir <- normalizePath(testthat::test_path("data"))

loaded_data_con <- setup_load(outcome = "Continuous")
defined_data_con <- setup_configure(loaded_data_con$data, loaded_data_con$treatment_df, "Continuous", "MD", "Placebo")
excluded_data_con <- setup_exclude(defined_data_con$non_covariate_data_all, defined_data_con$treatment_df, "Placebo", "Continuous", "MD", "random", c("Study01", "Study02", "Study03", "Study04"))
loaded_data_bin <- setup_load(outcome = "Binary")
defined_data_bin <- setup_configure(loaded_data_bin$data, loaded_data_bin$treatment_df, "Binary", "OR", "Placebo")

fitted_bayes_model <- bayes_model(defined_data_con$main_connected_data,
                                  loaded_data_con$treatment_df,
                                  "Continuous",
                                  "MD",
                                  "random",
                                  "Placebo",
                                  123)

fitted_baseline_model <- baseline_model(defined_data_con$main_connected_data,
                                        loaded_data_con$treatment_df,
                                        "Continuous",
                                        "MD",
                                        "Placebo",
                                        "random",
                                        "shared",
                                        123)

fitted_covariate_model <- covariate_model(defined_data_con$main_connected_data,
                                          defined_data_con$treatment_df,
                                          "Continuous",
                                          "MD",
                                          50,
                                          "random",
                                          "shared",
                                          "Continuous",
                                          "Placebo",
                                          123)

# fit models and save them so they can be reloaded in other tests
# my PC has a pecularity where the upload of temporary files fails
if (Sys.getenv("GITHUB_ACTIONS") == "true"){
  bayes_model_path <- tempfile(fileext = ".rds")
  baseline_model_path <-tempfile(fileext = ".rds")
  covariate_model_path <- tempfile(fileext = ".rds")
  save_file <- tempfile(fileext = ".rds")
} else {
  bayes_model_path <- testthat::test_path("temprds", "bayes.rds")
  baseline_model_path <- testthat::test_path("temprds", "baseline.rds")
  covariate_model_path <- testthat::test_path("temprds", "covariate.rds")
  save_file <- testthat::test_path("temprds", "save_file.rds")
}

app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
app$set_inputs(tabs = "setup")
app$click("setup_load-run")
app$wait_for_value(input = "setup_configure-ready")
app$click("setup_configure-run")
app$wait_for_value(output = "setup_exclude-exclusions_out")
app$set_inputs("setup_exclude-exclusions" = "Leo")
app$wait_for_value(input = "setup_exclude-complete")
app$click("bayes_model-run")
app$wait_for_value(input = "bayes_model-all-complete")
app$wait_for_value(input = "bayes_model-sub-complete")
app$get_download("core_save-save_session", filename = bayes_model_path)
app$stop()

app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
app$click("setup_load-run")
app$click("setup_configure-run")
app$click("baseline_model-run")
app$wait_for_value(input = "baseline_model-complete")
app$get_download("core_save-save_session", filename = baseline_model_path)
app$stop()

app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
app$click("setup_load-run")
app$click("setup_configure-run")
app$click("covariate_model-run")
app$wait_for_value(input = "covariate_model-complete")
app$get_download("core_save-save_session", filename = covariate_model_path)
app$stop()
