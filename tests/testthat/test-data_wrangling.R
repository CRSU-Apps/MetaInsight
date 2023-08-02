
test_that("find_all_treatments() finds all treatements for long-format data", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C"),
                     T = c("Egg", "Flour", "Egg", "Sugar", "Egg", "Butter", "Cinnamon"),
                     OtherText = c("A", "A", "B", "B", "C", "C", "C"))
  
  treatments <- find_all_treatments(data)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("find_all_treatments() finds all treatements for wide-format data", {
  data <- data.frame(Study = c("A", "B", "C"),
                     T.1 = c("Egg", "Egg", "Egg"),
                     OtherText.1 = c("A", "B", "C"),
                     T.2 = c("Flour", "Sugar", "Butter"),
                     OtherText.2 = c("A", "B", "C"),
                     T.3 = c(NA, NA, "Cinnamon"),
                     OtherText.3 = c(NA, NA, "C"))
  
  treatments <- find_all_treatments(data)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("vector_with_item_first() returns unchanged vector when intended first item not in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- vector_with_item_first(vector, "Potato")
  
  expect_equal(!!result, !!vector)
})

test_that("vector_with_item_first() returns unchanged vector when intended first item already first in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- vector_with_item_first(vector, "Egg")
  
  expect_equal(!!result, !!vector)
})

test_that("vector_with_item_first() returns ordered vector when intended first item is in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- vector_with_item_first(vector, "Sugar")
  
  expect_equal(!!result, c("Sugar", "Egg", "Flour", "Butter", "Cinnamon"))
})

test_that("create_treatment_ids() creates treatment list with firstreference treatment first", {
  treatment_ids <- create_treatment_ids(c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"), "Flour")
  
  expect_equal(
    !!treatment_ids,
    data.frame(
      Number = 1:5,
      Label = c("Flour", "Egg", "Sugar", "Butter", "Cinnamon")
    )
  )
})

test_that("find_expected_reference_treatment() returns NULL when no matches", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_null(!!result)
})

test_that("find_expected_reference_treatment() returns treatment when single match", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("find_expected_reference_treatment() returns first matching treatment when multiple matches", {
  vector <- c("Egg", "Flour", "No-Contact", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("find_expected_reference_treatment() returns NULL when no matches", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_null(!!result)
})

test_that("find_expected_reference_treatment() returns treatment when single match", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("find_expected_reference_treatment() returns first matching treatment when multiple matches", {
  vector <- c("Egg", "Flour", "No-Contact", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- find_expected_reference_treatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("replace_treatment_ids() updates treatment names to IDs for continuous long data", {
  data <- clean_data(read.csv("Cont_long.csv"))
  all_treatments <- find_all_treatments(data)
  treatment_ids <- create_treatment_ids(all_treatments, all_treatments[1])
  
  wrangled_data <- replace_treatment_ids(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T, c(1, 2, 3, 4, 1, 5, 1, 6),
               label = format_vector_to_string(wrangled_data$T))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "T"],
               data[, colnames(data) != "T"])
})

test_that("replace_treatment_ids() updates treatment names to IDs for continuous wide data", {
  data <- clean_data(read.csv("Cont_wide.csv"))
  all_treatments <- find_all_treatments(data)
  treatment_ids <- create_treatment_ids(all_treatments, all_treatments[1])
  
  wrangled_data <- replace_treatment_ids(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T.1, c(1, 2, 1),
               label = format_vector_to_string(wrangled_data$T.1))
  expect_equal(wrangled_data$T.2, c(3, 1, 4),
               label = format_vector_to_string(wrangled_data$T.2))
  expect_equal(wrangled_data$T.3, c(5, 6, NA),
               label = format_vector_to_string(wrangled_data$T.3))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, !(colnames(wrangled_data) %in% paste0("T.", 1:6))],
               data[, !(colnames(data) %in% paste0("T.", 1:6))])
})

test_that("replace_treatment_ids() updates treatment names to IDs for binary long data", {
  data <- clean_data(read.csv("Binary_long.csv"))
  all_treatments <- find_all_treatments(data)
  treatment_ids <- create_treatment_ids(all_treatments, all_treatments[1])
  
  wrangled_data <- replace_treatment_ids(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T, c(1, 2, 3, 4, 1, 5, 1, 6),
               label = format_vector_to_string(wrangled_data$T))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "T"],
               data[, colnames(data) != "T"])
})

test_that("replace_treatment_ids() updates treatment names to IDs for binary wide data", {
  data <- clean_data(read.csv("Binary_wide.csv"))
  all_treatments <- find_all_treatments(data)
  treatment_ids <- create_treatment_ids(all_treatments, all_treatments[1])
  
  wrangled_data <- replace_treatment_ids(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T.1, c(1, 2, 1),
               label = format_vector_to_string(wrangled_data$T.1))
  expect_equal(wrangled_data$T.2, c(3, 1, 4),
               label = format_vector_to_string(wrangled_data$T.2))
  expect_equal(wrangled_data$T.3, c(5, 6, NA),
               label = format_vector_to_string(wrangled_data$T.3))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, !(colnames(wrangled_data) %in% paste0("T.", 1:6))],
               data[, !(colnames(data) %in% paste0("T.", 1:6))])
})

test_that("add_study_ids() adds study IDs for continuous long data", {
  data <- clean_data(read.csv("Cont_long.csv"))
  
  wrangled_data <- add_study_ids(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("add_study_ids() adds study IDs for continuous wide data", {
  data <- clean_data(read.csv("Cont_wide.csv"))
  
  wrangled_data <- add_study_ids(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("add_study_ids() adds study IDs for binary long data", {
  data <- clean_data(read.csv("Binary_long.csv"))
  
  wrangled_data <- add_study_ids(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("add_study_ids() adds study IDs for binary wide data", {
  data <- clean_data(read.csv("Binary_wide.csv"))
  
  wrangled_data <- add_study_ids(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("reorder_columns() reorders columns for continuous long data", {
  data <- clean_data(read.csv("Cont_long.csv"))
  
  wrangled_data <- data %>%
    add_study_ids() %>%
    reorder_columns("Continuous")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "N", "Mean", "SD"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    expect_equal(wrangled_data[[col]], data[[col]])
  }
})

test_that("reorder_columns() reorders columns for continuous wide data", {
  data <- clean_data(read.csv("Cont_wide.csv"))
  
  wrangled_data <- data %>%
    add_study_ids() %>%
    reorder_columns("Continuous")
  
  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "N.1",
    "Mean.1",
    "SD.1",
    "T.2",
    "N.2",
    "Mean.2",
    "SD.2",
    "T.3",
    "N.3",
    "Mean.3",
    "SD.3"
  )
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    expect_equal(wrangled_data[[col]], data[[col]])
  }
})

test_that("reorder_columns() reorders columns for binary long data", {
  data <- clean_data(read.csv("Binary_long.csv"))
  
  wrangled_data <- data %>%
    add_study_ids() %>%
    reorder_columns("Binary")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "R", "N"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    expect_equal(wrangled_data[[col]], data[[col]])
  }
})

test_that("reorder_columns() reorders columns for binary wide data", {
  data <- clean_data(read.csv("Binary_wide.csv"))
  
  wrangled_data <- data %>%
    add_study_ids() %>%
    reorder_columns("Binary")
  
  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "R.1",
    "N.1",
    "T.2",
    "R.2",
    "N.2",
    "T.3",
    "R.3",
    "N.3"
  )
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    expect_equal(wrangled_data[[col]], data[[col]])
  }
})

test_that("wrangle_upload_data_to_app_data() wrangles continuous long data to be usable in the rest of the app", {
  data <- clean_data(read.csv("Cont_long.csv"))
  treatment_ids <- data %>%
    find_all_treatments() %>%
    create_treatment_ids()
  
  wrangled_data <- wrangle_upload_data_to_app_data(data, treatment_ids, "Continuous")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "N", "Mean", "SD"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col == "T") {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("wrangle_upload_data_to_app_data() wrangles continuous wide data to be usable in the rest of the app", {
  data <- clean_data(read.csv("Cont_wide.csv"))
  treatment_ids <- data %>%
    find_all_treatments() %>%
    create_treatment_ids()
  
  wrangled_data <- wrangle_upload_data_to_app_data(data, treatment_ids, "Continuous")

  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "N.1",
    "Mean.1",
    "SD.1",
    "T.2",
    "N.2",
    "Mean.2",
    "SD.2",
    "T.3",
    "N.3",
    "Mean.3",
    "SD.3"
  )

  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  ignored_columns <- paste0("T", c("", paste0(".", 1:6)))

  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col %in% ignored_columns) {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("wrangle_upload_data_to_app_data() wrangles binary long data to be usable in the rest of the app", {
  data <- clean_data(read.csv("Binary_long.csv"))
  treatment_ids <- data %>%
    find_all_treatments() %>%
    create_treatment_ids()
  
  wrangled_data <- wrangle_upload_data_to_app_data(data, treatment_ids, "Binary")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "R", "N"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col == "T") {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("wrangle_upload_data_to_app_data() wrangles binary wide data to be usable in the rest of the app", {
  data <- clean_data(read.csv("Binary_wide.csv"))
  treatment_ids <- data %>%
    find_all_treatments() %>%
    create_treatment_ids()
  
  wrangled_data <- wrangle_upload_data_to_app_data(data, treatment_ids, "Binary")
  
  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "R.1",
    "N.1",
    "T.2",
    "R.2",
    "N.2",
    "T.3",
    "R.3",
    "N.3"
  )
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  ignored_columns <- paste0("T", c("", paste0(".", 1:6)))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col %in% ignored_columns) {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})
