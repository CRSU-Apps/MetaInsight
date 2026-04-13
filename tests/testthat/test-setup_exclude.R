
test_that("setup_exclude produces errors for incorrect data types", {
  expect_error(setup_exclude("not_configured", c("Leo")), "configured_data must be of class configured_data")
  expect_error(setup_exclude(configured_data_con, 123), "exclusions must be of class character")

  expect_error(setup_exclude(configured_data_con, c("Study1000")), "exclusions must in the present in the loaded data")
})

test_that("setup_exclude produces data of the correct type", {
  result <- setup_exclude(configured_data_con, c("Leo"))

  expected_items <- c("treatments", "connected_data", "covariate", "freq", "outcome",
                       "outcome_measure", "effects", "ranking_option", "seed")

  expect_type(result, "list")
  expect_true(all(expected_items %in% names(result)))
  expect_s3_class(result$treatments, "data.frame")
  expect_type(result$reference_treatment, "character")
  expect_s3_class(result$connected_data, "data.frame")
  expect_type(result$covariate, "list")
  expect_type(result$covariate$column, "character")
  expect_type(result$covariate$name, "character")
  expect_type(result$covariate$type, "character")
  expect_type(result$freq, "list")
  expect_type(result$outcome, "character")
  expect_type(result$outcome_measure, "character")
  expect_type(result$effects, "character")
  expect_type(result$ranking_option, "character")
  expect_type(result$seed, "double")

})

test_that("setup_exclude_plot functions correctly with a single line per study", {
  exclusions <- c("Leo", "Constantine")
  result <- setup_exclude_plot(configured_data_con, exclusions)
  expect_match(result, "<svg")
  svg_doc <- xml2::as_xml_document(result)
  selected_lines <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g[contains(@style, 'opacity: 0.3')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )
  study_names <- xml2::xml_attr(selected_lines, "data-study-name")
  expect_setequal(exclusions, study_names)
})

test_that("setup_exclude_plot functions correctly with multiple lines per study", {
  exclusions <- c("Leo")
  load <- setup_load(file.path(test_data_dir, "Cont_long.csv"), "continuous")
  configured <- setup_configure(load, "the Great", "random", "MD", "good", 123)

  result <- setup_exclude_plot(configured, exclusions)
  expect_match(result, "<svg")
  svg_doc <- xml2::as_xml_document(result)
  selected_lines <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g[contains(@style, 'opacity: 0.3')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )
  study_names <- xml2::xml_attr(selected_lines, "data-study-name")
  expect_length(study_names, 3)
  expect_setequal(exclusions, unique(study_names))
})

test_that("setup_exclude removes the correct studies", {
  result <- setup_exclude(configured_data_con, c("Leo", "Constantine"))
  n_studies_all <- length(unique(configured_data_con$connected_data$Study))
  n_studies_freq <- length(unique(result$freq$pairwise$studlab))
  n_studies_sub <- length(unique(result$connected_data$Study))

  expect_false("Leo" %in% result$freq$pairwise$studlab)
  expect_false("Constantine" %in% result$freq$pairwise$studlab)
  expect_true("Justinian" %in% result$freq$pairwise$studlab)
  expect_equal(n_studies_all - 2, n_studies_freq)

  expect_false("Leo" %in% result$connected_data$Study)
  expect_false("Constantine" %in% result$connected_data$Study)
  expect_true("Justinian" %in% result$connected_data$Study)
  expect_equal(n_studies_all - 2, n_studies_sub)

})

test_that("setup_exclude updates interface and loads data into common correctly", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$upload_file("setup_load-file1" = minimal_data_path)
  app$set_inputs(tabs = "setup")
  app$click("setup_load-run")
  app$set_inputs(setupSel = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  app$click("setup_configure-run")
  app$set_inputs(tabs = "summary")
  app$wait_for_value(input = "setup_exclude-complete")

  # open the accordion
  app$click(selector = "#setup_exclude-collapse .accordion-button")
  app$wait_for_idle()

  # click on study lines
  click_setup_exclude(app, "Leo")
  click_setup_exclude(app, "Constantine")

  app$wait_for_idle()
  exclusions <- app$get_value(input = "setup_exclude-exclusions")
  expect_setequal(exclusions, c("Leo", "Constantine"))
  app$wait_for_value(input = "setup_exclude-complete", ignore = list(NULL, "", "initial"))

  # the JS updates the DOM rather than the output value, so this grabs as seen in the app
  svg_content <- app$get_js("document.getElementById('setup_exclude-interface').outerHTML")
  svg_doc <- xml2::read_xml(svg_content)
  selected_lines <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g[contains(@style, 'opacity: 0.3')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )
  study_names <- xml2::xml_attr(selected_lines, "data-study-name")
  expect_length(study_names, 2)
  expect_setequal(c("Leo", "Constantine"), unique(study_names))

  common <- app$get_value(export = "common")

  expect_type(common$subsetted_data$freq, "list")
  expect_type(common$subsetted_data$reference_treatment, "character")

  n_studies_all <- length(unique(common$configured_data$freq$pairwise$studlab))
  n_studies_sub_con <- length(unique(common$subsetted_data$connected_data$Study))
  n_studies_sub_freq <- length(unique(common$subsetted_data$freq$pairwise$studlab))

  expect_false("Leo" %in% common$subsetted_data$connected_data$Study)
  expect_false("Constantine" %in% common$subsetted_data$connected_data$Study)
  expect_true("Justinian" %in% common$subsetted_data$connected_data$Study)

  expect_false("Leo" %in% common$subsetted_data$freq$pairwise$studlab)
  expect_false("Constantine" %in% common$subsetted_data$freq$pairwise$studlab)
  expect_true("Justinian" %in% common$subsetted_data$freq$pairwise$studlab)
  expect_equal(n_studies_all - 2, n_studies_sub_freq)

  app$stop()
})

test_that("setup_exclude launches a note when reference_treatment_sub changes", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  app$upload_file("setup_load-file1" = file.path(test_data_dir, "Cont_long_continuous_cov.csv"))
  app$set_inputs(tabs = "setup")
  app$click("setup_load-run")
  app$set_inputs("setupSel" = "setup_configure")
  app$wait_for_value(input = "setup_configure-ready")
  # this uses unformatted treatments initially
  app$set_inputs("setup_configure-reference_treatment" = "the Little")
  app$click("setup_configure-run")
  app$wait_for_value(input = "setup_exclude-complete")

  # open the accordion
  app$click(selector = "#setup_exclude-collapse .accordion-button")
  app$wait_for_idle()

  # click on study lines
  click_setup_exclude(app, "Leo")

  app$wait_for_idle()
  exclusions <- app$get_value(input = "setup_exclude-exclusions")
  expect_setequal(exclusions, c("Leo"))
  app$wait_for_value(input = "setup_exclude-complete", ignore = list(NULL, "", "initial"))
  common <- app$get_value(export = "common")
  expect_equal(common$subsetted_data$reference_treatment, "the_Great")
  expect_false(common$subsetted_data$reference_treatment == common$configured_data$reference_treatment)

  logger <- app$get_value(export = "logger")
  expect_true(grepl("*has been changed to the_Great*", logger))

  app$stop()
})

test_that("setup_exclude restores after loading", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "metainsight"), timeout = 30000)
  # already has Minerva excluded
  reload_app(app, config_path)
  # open the accordion
  app$click(selector = "#setup_exclude-collapse .accordion-button")
  app$wait_for_idle()

  # the JS updates the DOM rather than the output value, so this grabs as seen in the app
  svg_content <- app$get_js("document.getElementById('setup_exclude-interface').outerHTML")
  svg_doc <- xml2::read_xml(svg_content)
  selected_lines <- xml2::xml_find_all(
    svg_doc,
    ".//d1:g[contains(@style, 'opacity: 0.3')]",
    ns = c(d1 = "http://www.w3.org/2000/svg")
  )
  study_names <- xml2::xml_attr(selected_lines, "data-study-name")
  expect_equal(c("Minerva"), study_names)

  app$stop()
})


