test_that("rep_markdown produces a renderable .Rmd when no analysis has been conducted", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("rep_markdown-dlRMD")
  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  writeLines(lines, sess_file)
  rmarkdown::render(sess_file)
  html_file <- gsub("Rmd","html", sess_file)
  expect_gt(file.info(html_file)$size, 1000)
  app$stop()
})

test_that("rep_markdown produces a renderable .Rmd file after an analysis", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"))

  expected_chunks <- 1 #intro

  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_load")
  app$click("setup_load-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(setupSel = "setup_define")
  app$click("setup_define-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(tabs = "summary")
  app$set_inputs("summary_exclude-exclusions" = c("Study01", "Study25"))
  app$wait_for_value(input = "summary_exclude-complete")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(main = "Results")

  app$set_inputs(summarySel = "summary_char")
  app$click("summary_char-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(summarySel = "summary_study")
  app$click("summary_study-run")
  expected_chunks <- expected_chunks + 1

  app$set_inputs(summarySel = "summary_network")
  app$click("summary_network-run")
  expected_chunks <- expected_chunks + 3

  app$set_inputs(tabs = "freq")
  app$set_inputs(freqSel = "freq_forest")
  app$click("freq_forest-run")
  app$wait_for_value(input = "freq_forest-complete")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_compare")
  app$click("freq_compare-run")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_inconsistent")
  app$click("freq_inconsistent-run")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(freqSel = "freq_summary")
  app$click("freq_summary-run")
  expected_chunks <- expected_chunks + 2

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("rep_markdown-dlRMD")

  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  chunks <- sum(grepl("```\\{r", lines))
  expect_equal(chunks, expected_chunks)
  rmarkdown::render(sess_file)
  html_file <- gsub("Rmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 100000)
  app$stop()
})
