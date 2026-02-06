# The aim here is to process the data once at the start so it can be used downstream
test_data_dir <- normalizePath(testthat::test_path("data"))

loaded_data_con <- setup_load(outcome = "continuous")
configured_data_con <- setup_configure(loaded_data_con, "Placebo", "random", "MD", "good", 123)
excluded_data_con <- setup_exclude(configured_data_con, c("Study01", "Study02", "Study03", "Study04"))
loaded_data_bin <- setup_load(outcome = "binary")
configured_data_bin <- setup_configure(loaded_data_bin, "Placebo", "random", "OR", "good", 123)

fitted_bayes_model <- bayes_model(configured_data_con)
fitted_baseline_model <- baseline_model(configured_data_con, "shared")
fitted_covariate_model <- covariate_model(configured_data_con, 50, "shared")

# fit models and save them so they can be reloaded in other tests
# my PC has a pecularity where the upload of temporary files fails
if (Sys.getenv("GITHUB_ACTIONS") == "true"){
  bayes_model_path <- tempfile(fileext = ".rds")
  baseline_model_path <-tempfile(fileext = ".rds")
  covariate_model_path <- tempfile(fileext = ".rds")
  save_file <- tempfile(fileext = ".rds")
} else {
  rds_path <- normalizePath(testthat::test_path("temprds"))
  bayes_model_path <-  file.path(rds_path, "bayes.rds")
  baseline_model_path <- file.path(rds_path, "baseline.rds")
  covariate_model_path <- file.path(rds_path, "covariate.rds")
  save_file <- file.path(rds_path, "save_file.rds")
}

app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long.csv"))
app$set_inputs(tabs = "setup")
app$click("setup_load-run")
app$set_inputs("setupSel" = "setup_configure")
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
app$set_inputs(tabs = "covariate")
app$set_inputs(covariateSel = "covariate_model")
app$click("covariate_model-run")
app$wait_for_value(input = "covariate_model-complete")
app$get_download("core_save-save_session", filename = covariate_model_path)
app$stop()
