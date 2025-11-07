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

test_that("rep_markdown produces a renderable .Rmd file after an analysis", {
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

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  quarto::quarto_render(sess_file)
  html_file <- gsub("qmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)
  app$stop()
})
