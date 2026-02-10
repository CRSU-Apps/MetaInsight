test_that("rep_markdown produces a renderable .qmd when no analysis has been conducted", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$set_inputs("rep_markdown-file_type" = ".qmd")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  sess_file <- app$get_download("rep_markdown-dlRMD")
  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  writeLines(lines, sess_file)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd","html", sess_file)
  expect_gt(file.info(html_file)$size, 1000)
  app$stop()
})

test_that("rep_markdown produces can render a qmd to html when no analysis has been conducted", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  html_file <- app$get_download("rep_markdown-dlRMD")
  expect_false(is.null(html_file))
  expect_gt(file.info(html_file)$size, 1000)
  app$stop()
})

test_that("rep_markdown produces a renderable .Rmd file after a frequentist analysis", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)

  expected_chunks <- 1 #intro

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(tabs = "summary")
  app$set_inputs("setup_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "setup_exclude-complete")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(main = "Results", wait_ = FALSE)

  app$set_inputs(summarySel = "summary_char", wait_ = FALSE)
  app$click("summary_char-run")
  app$wait_for_value(output = "summary_char-table")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(summarySel = "summary_study")
  app$click("summary_study-run")
  app$wait_for_value(output = "summary_study-plot")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(summarySel = "summary_network")
  app$click("summary_network-run")
  app$wait_for_value(output = "summary_network-plot_sub")
  app$wait_for_value(output = "summary_network-plot_all")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_forest")
  app$click("freq_forest-run")
  app$wait_for_value(output = "freq_forest-plot_all")
  app$wait_for_value(output = "freq_forest-plot_sub")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_compare")
  app$click("freq_compare-run")
  app$wait_for_value(output = "freq_compare-table_all")
  app$wait_for_value(output = "freq_compare-table_sub")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_inconsistent")
  app$click("freq_inconsistent-run")
  app$wait_for_value(output = "freq_inconsistent-table_all")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_summary")
  app$click("freq_summary-run")
  app$wait_for_value(output = "freq_summary-plot_all")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$set_inputs("rep_markdown-file_type" = ".qmd")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  sess_file <- app$get_download("rep_markdown-dlRMD")

  app$stop()

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)

})

test_that("rep_markdown produces a renderable .Rmd file after a bayesian analysis", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = bayes_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "bayes")

  #intro + setup_load + setup_configure + setup_exclude + bayes_model (3)
  expected_chunks <- 7

  app$set_inputs(bayesSel = "bayes_forest")
  app$click("bayes_forest-run")
  # store to compare later
  forest_all_app <- app$wait_for_value(output = "bayes_forest-all-plot")
  forest_sub_app <- app$wait_for_value(output = "bayes_forest-sub-plot")

  expected_chunks <- expected_chunks + 2

  app$set_inputs(bayesSel = "bayes_compare")
  app$click("bayes_compare-run")
  app$wait_for_value(output = "bayes_compare-all-table")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(bayesSel = "bayes_deviance")
  app$click("bayes_deviance-run")
  app$wait_for_value(input = "bayes_deviance-complete")
  app$wait_for_value(output = "bayes_deviance-all-stem")
  expected_chunks <- expected_chunks + 7

  app$set_inputs(bayesSel = "bayes_ranking")
  app$click("bayes_ranking-run")
  app$wait_for_value(output = "bayes_ranking-all-forest")
  expected_chunks <- expected_chunks + 9

  app$set_inputs(bayesSel = "bayes_mcmc")
  app$click("bayes_mcmc-run")
  app$wait_for_value(input = "bayes_mcmc_all-complete")
  app$wait_for_value(input = "bayes_mcmc_sub-complete")
  expected_chunks <- expected_chunks + 7

  app$set_inputs(bayesSel = "bayes_details")
  app$click("bayes_details-run")
  app$wait_for_value(output = "bayes_details-bayes-mcmc")
  expected_chunks <- expected_chunks + 8

  app$set_inputs(bayesSel = "bayes_results")
  app$click("bayes_results-run")
  app$wait_for_idle()
  app$wait_for_value(output = "bayes_results-all-statistics")
  expected_chunks <- expected_chunks + 6

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$set_inputs("rep_markdown-file_type" = ".qmd")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  sess_file <- app$get_download("rep_markdown-dlRMD")

  app$stop()

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)

  # test that results are reproducible by comparing forest plots
  html_text <- extract_svg_text_from_html(html_file)
  forest_all_text <- extract_svg_text_from_svg(forest_all_app$html)
  forest_sub_text <- extract_svg_text_from_svg(forest_sub_app$html)
  expect_true(identical(html_text[[1]], forest_all_text))
  expect_true(identical(html_text[[2]], forest_sub_text))

})

test_that("rep_markdown produces a renderable .Rmd file after a covariate analysis", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = covariate_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "covariate")

  # ensure inputs update
  app$set_inputs(covariateSel = "covariate_model")

  # intro + setup_load + setup_configure + setup_exclude + covariate_model
  expected_chunks <- 5

  app$set_inputs(covariateSel = "covariate_summary")
  app$click("covariate_summary-run")
  app$wait_for_value(output = "covariate_summary-plot")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(covariateSel = "covariate_regression")
  # needed to avoid error
  app$set_inputs("covariate_regression-covariate-credible" = FALSE)
  app$click("covariate_regression-covariate-run")
  app$wait_for_value(input = "covariate_regression-complete")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(covariateSel = "covariate_forest")
  app$click("covariate_forest-run")
  forest_app <- app$wait_for_value(output = "covariate_forest-covariate-plot")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(covariateSel = "covariate_comparison")
  app$click("covariate_comparison-run")
  app$wait_for_value(output = "covariate_comparison-covariate-table")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(covariateSel = "covariate_deviance")
  app$click("covariate_deviance-run")
  app$wait_for_value(input = "covariate_deviance-complete")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(covariateSel = "covariate_ranking")
  app$click("covariate_ranking-run")
  app$wait_for_value(output = "covariate_ranking-all-forest")
  expected_chunks <- expected_chunks + 5

  app$set_inputs(covariateSel = "covariate_mcmc")
  app$click("covariate_mcmc-run")
  app$wait_for_value(input = "covariate_mcmc-complete")
  expected_chunks <- expected_chunks + 4

  app$set_inputs(covariateSel = "covariate_details")
  app$click("covariate_details-run")
  app$wait_for_value(output = "covariate_details-covariate-mcmc")
  expected_chunks <- expected_chunks + 5

  app$set_inputs(covariateSel = "covariate_results")
  app$click("covariate_results-run")
  app$wait_for_idle()
  app$wait_for_value(output = "covariate_results-all-statistics")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$set_inputs("rep_markdown-file_type" = ".qmd")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  sess_file <- app$get_download("rep_markdown-dlRMD")

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)

  # test that results are reproducible by comparing forest plots
  html_text <- extract_svg_text_from_html(html_file)
  forest_text <- extract_svg_text_from_svg(forest_app$html)
  # 3rd svg plot, following module order in global.R
  expect_true(identical(html_text[[3]], forest_text))

  app$stop()

})

test_that("rep_markdown produces a renderable .Rmd file after a baseline analysis", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 60000)

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = baseline_model_path)
  app$click("setup_reload-goLoad_session")
  app$set_inputs(tabs = "baseline")

  #intro + setup_load + setup_configure + setup_exclude + baseline_model
  expected_chunks <- 5

  app$set_inputs(baselineSel = "baseline_summary")
  app$click("baseline_summary-run")
  app$wait_for_value(output = "baseline_summary-plot")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(baselineSel = "baseline_regression")
  # needed to avoid error
  app$set_inputs("baseline_regression-baseline-credible" = FALSE)
  app$click("baseline_regression-baseline-run")
  app$wait_for_value(input = "baseline_regression-complete")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(baselineSel = "baseline_forest")
  app$click("baseline_forest-run")
  forest_app <- app$wait_for_value(output = "baseline_forest-baseline-plot")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(baselineSel = "baseline_comparison")
  app$click("baseline_comparison-run")
  app$wait_for_value(output = "baseline_comparison-baseline-table")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(baselineSel = "baseline_deviance")
  app$click("baseline_deviance-run")
  app$wait_for_value(input = "baseline_deviance-complete")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(baselineSel = "baseline_ranking")
  app$click("baseline_ranking-run")
  app$wait_for_value(output = "baseline_ranking-all-forest")
  expected_chunks <- expected_chunks + 5

  app$set_inputs(baselineSel = "baseline_mcmc")
  app$click("baseline_mcmc-run")
  app$wait_for_value(input = "baseline_mcmc-complete")
  expected_chunks <- expected_chunks + 4

  app$set_inputs(baselineSel = "baseline_details")
  app$click("baseline_details-run")
  app$wait_for_value(output = "baseline_details-baseline-mcmc")
  expected_chunks <- expected_chunks + 5

  app$set_inputs(baselineSel = "baseline_results")
  app$click("baseline_results-run")
  app$wait_for_idle()
  app$wait_for_value(output = "baseline_results-all-statistics")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  app$set_inputs("rep_markdown-file_type" = ".qmd")
  app$click("rep_markdown-download")
  app$wait_for_value(input = "rep_markdown-complete")
  sess_file <- app$get_download("rep_markdown-dlRMD")

  app$stop()

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)

  # test that results are reproducible by comparing forest plots
  html_text <- extract_svg_text_from_html(html_file)
  forest_text <- extract_svg_text_from_svg(forest_app$html)
  # 3rd svg plot, following module order in global.R
  expect_true(identical(html_text[[3]], forest_text))

})

