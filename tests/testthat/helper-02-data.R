# The aim here is to process the data once at the start so it can be used downstream
test_data_dir <- normalizePath(testthat::test_path("data"))
minimal_data_path <- system.file("extdata", "continuous_minimal.csv", package = "metainsight")

loaded_data_bin <- setup_load(file.path(test_data_dir, "Binary_long_continuous_cov.csv"), outcome = "binary")
configured_data_bin <- setup_configure(loaded_data_bin, "the Great", "random", "OR", "good", 123)

options(shinytest2.load_timeout=60000)

if (Sys.getenv("LOCAL") == "true"){
  rds_path <- normalizePath(testthat::test_path("saved_files"))
  if (!file.exists(rds_path)) dir.create(rds_path)
  config_path <- file.path(rds_path, "config.rds")
  bayes_model_path <- file.path(rds_path, "bayes.rds")
  baseline_model_path <- file.path(rds_path, "baseline.rds")
  covariate_model_path <- file.path(rds_path, "covariate.rds")
  save_file <- file.path(rds_path, "save_file.rds")
} else {
  config_path <- tempfile(fileext = ".rds")
  bayes_model_path <- tempfile(fileext = ".rds")
  baseline_model_path <-tempfile(fileext = ".rds")
  covariate_model_path <- tempfile(fileext = ".rds")
  save_file <- tempfile(fileext = ".rds")
}

on_cran <- ((Sys.getenv("GITHUB_ACTIONS") == "true") + (Sys.getenv("LOCAL") == "true")) == 0

if (!on_cran){
  if (!file.exists(config_path) || !file.exists(bayes_model_path)){
    app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
    app$upload_file("setup_load-file1" = minimal_data_path)
    app$set_inputs(tabs = "setup")
    app$click("setup_load-run")
    app$set_inputs("setupSel" = "setup_configure")
    app$wait_for_value(input = "setup_configure-ready")
    app$set_inputs("setup_configure-reference_treatment" = "the Great", wait_ = FALSE)
    app$set_inputs("setup_configure-seed" = 123, wait_ = FALSE)
    app$click("setup_configure-run")
    app$wait_for_value(input = "setup_exclude-complete")
    # open the accordion
    app$click(selector = "#setup_exclude-collapse .accordion-button")
    app$wait_for_idle()
    click_setup_exclude(app, "Minerva")
    app$wait_for_value(input = "setup_exclude-complete", ignore = list(NULL, "", "initial"))
    app$set_inputs(tabs = "covariate")
    app$set_inputs(covariateSel = "covariate_model")
    app$wait_for_idle()
    app$get_download("core_save-save_session", filename = config_path)
    app$click("bayes_model-run")
    app$wait_for_value(input = "bayes_model-all-complete")
    app$wait_for_value(input = "bayes_model-sub-complete")
    app$get_download("core_save-save_session", filename = bayes_model_path)
    app$stop()
  }

  if (!file.exists(baseline_model_path)){
    app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
    app$upload_file("setup_load-file1" = minimal_data_path)
    app$click("setup_load-run")
    app$set_inputs("setupSel" = "setup_configure")
    app$wait_for_value(input = "setup_configure-ready")
    app$set_inputs("setup_configure-seed" = 123, wait_ = FALSE)
    app$click("setup_configure-run")
    app$click("baseline_model-run")
    app$wait_for_value(input = "baseline_model-complete")
    app$get_download("core_save-save_session", filename = baseline_model_path)
    app$stop()
  }

  if (!file.exists(covariate_model_path)){
    app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
    app$upload_file("setup_load-file1" = minimal_data_path)
    app$click("setup_load-run")
    app$set_inputs("setupSel" = "setup_configure")
    app$wait_for_value(input = "setup_configure-ready")
    app$set_inputs("setup_configure-seed" = 123, wait_ = FALSE)
    app$click("setup_configure-run")
    app$wait_for_value(input = "setup_exclude-complete")
    app$set_inputs(tabs = "covariate")
    app$set_inputs(covariateSel = "covariate_model")
    app$click("covariate_model-run")
    app$wait_for_value(input = "covariate_model-complete")
    app$get_download("core_save-save_session", filename = covariate_model_path)
    app$stop()
  }

  configured_data_con <- readRDS(config_path)$configured_data
  excluded_data_con <- readRDS(config_path)$subsetted_data

  fitted_bayes_model <- readRDS(bayes_model_path)$bayes_model_all
  fitted_baseline_model <- readRDS(baseline_model_path)$baseline_model
  fitted_covariate_model <- readRDS(covariate_model_path)$covariate_model

}

if (on_cran){
  loaded_data_con <- setup_load(minimal_data_path, outcome = "continuous")
  configured_data_con <- setup_configure(loaded_data_con, "the Great", "random", "MD", "good", 123)
  excluded_data_con <- setup_exclude(configured_data_con, c("Minerva"))

  fitted_bayes_model <- bayes_model(configured_data_con, 100, 100)
  fitted_baseline_model <- baseline_model(configured_data_con, "shared", 120, 120, 12)
  fitted_covariate_model <- covariate_model(configured_data_con, 97, "shared", NULL, 100, 100)
}

n_trt_all <- nrow(configured_data_con$treatments)
n_comparisons_all <- length(netmeta::netconnection(configured_data_con$freq$netmeta)$comparisons)
n_trt_sub <- nrow(excluded_data_con$treatments)
n_comparisons_sub <- length(netmeta::netconnection(excluded_data_con$freq$netmeta)$comparisons)
n_trt_bin <- nrow(configured_data_bin$treatments)
n_comparisons_bin <- length(netmeta::netconnection(configured_data_bin$freq$netmeta)$comparisons)

