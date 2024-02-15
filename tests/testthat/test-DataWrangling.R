
test_that("FindAllTreatments() finds all treatements for long-format data", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C"),
                     T = c("Egg", "Flour", "Egg", "Sugar", "Egg", "Butter", "Cinnamon"),
                     OtherText = c("A", "A", "B", "B", "C", "C", "C"))
  
  treatments <- FindAllTreatments(data)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("FindAllTreatments() finds all treatements for wide-format data", {
  data <- data.frame(Study = c("A", "B", "C"),
                     T.1 = c("Egg", "Egg", "Egg"),
                     OtherText.1 = c("A", "B", "C"),
                     T.2 = c("Flour", "Sugar", "Butter"),
                     OtherText.2 = c("A", "B", "C"),
                     T.3 = c(NA, NA, "Cinnamon"),
                     OtherText.3 = c(NA, NA, "C"))
  
  treatments <- FindAllTreatments(data)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("VectorWithItemFirst() returns unchanged vector when intended first item not in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- VectorWithItemFirst(vector, "Potato")
  
  expect_equal(!!result, !!vector)
})

test_that("VectorWithItemFirst() returns unchanged vector when intended first item already first in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- VectorWithItemFirst(vector, "Egg")
  
  expect_equal(!!result, !!vector)
})

test_that("VectorWithItemFirst() returns ordered vector when intended first item is in vector", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- VectorWithItemFirst(vector, "Sugar")
  
  expect_equal(!!result, c("Sugar", "Egg", "Flour", "Butter", "Cinnamon"))
})

test_that("CreateTreatmentIds() creates treatment list with reference treatment first", {
  treatment_ids <- CreateTreatmentIds(c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"), "Flour")
  
  expect_equal(
    !!treatment_ids,
    data.frame(
      Number = 1:5,
      Label = c("Flour", "Egg", "Sugar", "Butter", "Cinnamon")
    )
  )
})

test_that("FindExpectedReferenceTreatment() returns NULL when no matches", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_null(!!result)
})

test_that("FindExpectedReferenceTreatment() returns treatment when single match", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("FindExpectedReferenceTreatment() returns first matching treatment when multiple matches", {
  vector <- c("Egg", "Flour", "No-Contact", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("FindExpectedReferenceTreatment() returns NULL when no matches", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_null(!!result)
})

test_that("FindExpectedReferenceTreatment() returns treatment when single match", {
  vector <- c("Egg", "Flour", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that("FindExpectedReferenceTreatment() returns first matching treatment when multiple matches", {
  vector <- c("Egg", "Flour", "No-Contact", "Sugar", "Butter", "Placebo", "Cinnamon")
  
  result <- FindExpectedReferenceTreatment(vector)
  
  expect_equal(!!result, "Placebo")
})

test_that(".FixColumnNameCases() fixes cases for continuous long data", {
  data <- CleanData(read.csv("Cont_long.csv"))
  names(data) <- c("sTuDy", "t", "n", "mEaN", "sD")
  allowed_names = c("Study", "T", "N", "Mean", "SD")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Continuous")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for continuous wide data", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  
  arm_fields = c("t", "n", "mEaN", "sD")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"), paste0(arm_fields, ".3"))
  
  allowed_arm_fields = c("T", "N", "Mean", "SD")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"), paste0(allowed_arm_fields, ".3"))
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Continuous")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary long data", {
  data <- CleanData(read.csv("Binary_long.csv"))
  names(data) <- c("sTuDy", "t", "r", "n")
  allowed_names = c("Study", "T", "R", "N")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Binary")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary wide data", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  
  arm_fields = c("t", "r", "n")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"), paste0(arm_fields, ".3"))
  
  allowed_arm_fields = c("T", "R", "N")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"), paste0(allowed_arm_fields, ".3"))
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Binary")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that("ReplaceTreatmentIds() updates treatment names to IDs for continuous long data", {
  data <- CleanData(read.csv("Cont_long.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T, c(1, 2, 3, 4, 1, 5, 1, 6),
               label = format_vector_to_string(wrangled_data$T))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "T"],
               data[, colnames(data) != "T"])
})

test_that("ReplaceTreatmentIds() updates treatment names to IDs for continuous wide data", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
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

test_that("ReplaceTreatmentIds() updates treatment names to IDs for binary long data", {
  data <- CleanData(read.csv("Binary_long.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
  expect_equal(colnames(wrangled_data), colnames(data),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$T, c(1, 2, 3, 4, 1, 5, 1, 6),
               label = format_vector_to_string(wrangled_data$T))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "T"],
               data[, colnames(data) != "T"])
})

test_that("ReplaceTreatmentIds() updates treatment names to IDs for binary wide data", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
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

test_that("AddStudyIds() adds study IDs for continuous long data", {
  data <- CleanData(read.csv("Cont_long.csv"))
  
  wrangled_data <- AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("AddStudyIds() adds study IDs for continuous wide data", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  
  wrangled_data <- AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("AddStudyIds() adds study IDs for binary long data", {
  data <- CleanData(read.csv("Binary_long.csv"))
  
  wrangled_data <- AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("AddStudyIds() adds study IDs for binary wide data", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  
  wrangled_data <- AddStudyIds(data)
  
  expect_equal(colnames(wrangled_data), c(colnames(data), "StudyID"),
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Other columns unchanged
  expect_equal(wrangled_data[, colnames(wrangled_data) != "StudyID"],
               data[, colnames(data) != "StudyID"])
})

test_that("ReorderColumns() reorders columns for continuous long data", {
  data <- CleanData(read.csv("Cont_long.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "N", "Mean", "SD"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    expect_equal(wrangled_data[[col]], data[[col]])
  }
})

test_that("ReorderColumns() reorders columns for continuous wide data", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
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

test_that("ReorderColumns() reorders columns for binary long data", {
  data <- CleanData(read.csv("Binary_long.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Binary")
  
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

test_that("ReorderColumns() reorders columns for binary wide data", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Binary")
  
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

test_that("WrangleUploadData() wrangles continuous long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("Cont_long.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Continuous")
  
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

test_that("WrangleUploadData() wrangles continuous wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("Cont_wide.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Continuous")

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

test_that("WrangleUploadData() wrangles binary long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("Binary_long.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Binary")
  
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

test_that("WrangleUploadData() wrangles binary wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("Binary_wide.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Binary")
  
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
