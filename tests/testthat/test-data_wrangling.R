
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

test_that("FindAllTreatments() finds all treatements for long-format data with treatment IDs", {
  data <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C"),
    T = c(1, 2, 1, 3, 1, 4, 5),
    OtherText = c("A", "A", "B", "B", "C", "C", "C")
  )
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  )
  
  treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("FindAllTreatments() finds all treatements for wide-format data with treatment IDs", {
  data <- data.frame(
    Study = c("A", "B", "C"),
    T.1 = c(1, 1, 1),
    OtherText.1 = c("A", "B", "C"),
    T.2 = c(2, 3, 4),
    OtherText.2 = c("A", "B", "C"),
    T.3 = c(NA, NA, 5),
    OtherText.3 = c(NA, NA, "C")
  )
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  )
  
  treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids)
  
  expect_equal(!!treatments, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"))
})

test_that("FindAllTreatments() finds all treatements for study for long-format data with treatment IDs", {
  data <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C"),
    T = c(1, 2, 1, 3, 1, 4, 5),
    OtherText = c("A", "A", "B", "B", "C", "C", "C")
  )
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  )
  
  treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids, study = "C")
  
  expect_equal(!!treatments, c("Egg", "Butter", "Cinnamon"))
})

test_that("FindAllTreatments() finds all treatements for study for wide-format data with treatment IDs", {
  data <- data.frame(
    Study = c("A", "B", "C"),
    T.1 = c(1, 1, 1),
    OtherText.1 = c("A", "B", "C"),
    T.2 = c(2, 3, 4),
    OtherText.2 = c("A", "B", "C"),
    T.3 = c(NA, NA, 5),
    OtherText.3 = c(NA, NA, "C")
  )
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Egg", "Flour", "Sugar", "Butter", "Cinnamon")
  )
  
  treatments <- FindAllTreatments(data = data, treatment_ids = treatment_ids, study = "C")
  
  expect_equal(!!treatments, c("Egg", "Butter", "Cinnamon"))
})

test_that("FindStudiesIncludingTreatments() finds all studies containing treatments for long data", {
  data <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
    T = c("Paracetamol", "Exercise", "Sleep", "Alcohol", "Bacon", "Denial", "Paracetamol", "Denial", "Exercise")
  )
  
  studies <- FindStudiesIncludingTreatments(data = data, treatments = c("Paracetamol", "Exercise"))
  
  expect_equal(sort(studies), c("A", "C", "D"), label = format_vector_to_string(sort(studies)))
})

test_that("FindStudiesIncludingTreatments() finds all studies containing treatments for wide data", {
  data <- data.frame(
    Study = c("A", "B", "C", "D"),
    T.1 = c("Paracetamol", "Sleep", "Bacon", "Denial"),
    T.2 = c("Exercise", "Alcohol", "Denial", "Exercise"),
    T.3 = c(NA, NA, "Paracetamol", NA)
  )
  
  studies <- FindStudiesIncludingTreatments(data = data, treatments = c("Paracetamol", "Exercise"))
  
  expect_equal(sort(studies), c("A", "C", "D"), label = format_vector_to_string(sort(studies)))
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

test_that(".FixColumnNameCases() fixes cases for continuous long data with covariate", {
  data <- CleanData(read.csv("data/Cont_long_continuous_cov.csv"))
  names(data) <- c("sTuDy", "t", "n", "mEaN", "sD", "CoVaR.age")
  allowed_names = c("Study", "T", "N", "Mean", "SD", "covar.age")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Continuous")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for continuous wide data with covariate", {
  data <- CleanData(read.csv("data/Cont_wide_continuous_cov.csv"))
  
  arm_fields = c("t", "n", "mEaN", "sD")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"), paste0(arm_fields, ".3"), "CoVaR.age")
  
  allowed_arm_fields = c("T", "N", "Mean", "SD")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"), paste0(allowed_arm_fields, ".3"), "covar.age")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Continuous")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary long data with covariate", {
  data <- CleanData(read.csv("data/Binary_long_continuous_cov.csv"))
  names(data) <- c("sTuDy", "t", "r", "n", "CoVaR.age")
  allowed_names = c("Study", "T", "R", "N", "covar.age")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Binary")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that(".FixColumnNameCases() fixes cases for binary wide data with covariate", {
  data <- CleanData(read.csv("data/Binary_wide_continuous_cov.csv"))
  
  arm_fields = c("t", "r", "n")
  names(data) <- c("sTuDy", paste0(arm_fields, ".1"), paste0(arm_fields, ".2"), paste0(arm_fields, ".3"), "CoVaR.age")
  
  allowed_arm_fields = c("T", "R", "N")
  allowed_names = c("Study", paste0(allowed_arm_fields, ".1"), paste0(allowed_arm_fields, ".2"), paste0(allowed_arm_fields, ".3"), "covar.age")
  
  expect(all(!names(data) %in% allowed_names), failure_message = "Column names were not setup for the test correctly.")
  
  wrangled_data <- .FixColumnNameCases(data, "Binary")
  
  expect_equal(colnames(wrangled_data), allowed_names,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(allowed_names))
})

test_that("ReplaceTreatmentIds() updates treatment names to IDs for continuous long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
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
  data <- CleanData(read.csv("data/Cont_wide.csv"))
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
  data <- CleanData(read.csv("data/Binary_long.csv"))
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
  data <- CleanData(read.csv("data/Binary_wide.csv"))
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

test_that("ReinstateTreatmentIds() reinstates treatment IDs for long data", {
  data <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
    T = c(1, 2, 3, 4, 5, 6, 1, 6, 2)
  )
  treatment_ids = data.frame(
    Label = c("Paracetamol", "Exercise", "Sleep", "Alcohol", "Bacon", "Denial"),
    Number = 1:6
  )
  
  new_data <- ReinstateTreatmentIds(data = data, treatment_ids = treatment_ids)
  expected <- data.frame(
    Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
    T = c("Paracetamol", "Exercise", "Sleep", "Alcohol", "Bacon", "Denial", "Paracetamol", "Denial", "Exercise")
  )
  
  expect_equal(!!new_data, !!expected)
})

test_that("ReinstateTreatmentIds() reinstates treatment IDs for wide data", {
  data <- data.frame(
    Study = c("A", "B", "C", "D"),
    T.1 = c(1, 3, 5, 6),
    T.2 = c(2, 4, 6, 2),
    T.3 = c(NA, NA, 1, NA)
  )
  treatment_ids = data.frame(
    Label = c("Paracetamol", "Exercise", "Sleep", "Alcohol", "Bacon", "Denial"),
    Number = 1:6
  )
  
  new_data <- ReinstateTreatmentIds(data = data, treatment_ids = treatment_ids)
  expected <- data.frame(
    Study = c("A", "B", "C", "D"),
    T.1 = c("Paracetamol", "Sleep", "Bacon", "Denial"),
    T.2 = c("Exercise", "Alcohol", "Denial", "Exercise"),
    T.3 = c(NA, NA, "Paracetamol", NA)
  )
  
  expect_equal(!!new_data, !!expected)
})

test_that("AddStudyIds() adds study IDs for continuous long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  
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
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  
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
  data <- CleanData(read.csv("data/Binary_long.csv"))
  
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
  data <- CleanData(read.csv("data/Binary_wide.csv"))
  
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

test_that(".ContinuousOrder() creates the correct ordering", {
  expected_order <- c("StudyID", "Study", "T", "N", "Mean", "SD", "T.1", "N.1", "Mean.1", "SD.1",
                      "T.2", "N.2", "Mean.2", "SD.2", "T.3", "N.3", "Mean.3", "SD.3", "RoB", "Indirectness")
  expect_equal(expected_order, .ContinuousOrder(3))
})

test_that(".BinaryOrder() creates the correct ordering", {
  expected_order <- c("StudyID", "Study", "T", "R", "N", "T.1", "R.1", "N.1",
                      "T.2", "R.2", "N.2", "T.3", "R.3", "N.3", "RoB", "Indirectness")
  expect_equal(expected_order, .BinaryOrder(3))
})

test_that("ReorderColumns() reorders columns for continuous long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
  retained_columns <- c(
    "Study",
    "T",
    "N",
    "Mean",
    "SD"
  )
  
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("ReorderColumns() reorders columns for continuous wide data", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
  retained_columns <- c(
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
  
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("ReorderColumns() reorders columns for binary long data", {
  data <- CleanData(read.csv("data/Binary_long.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Binary")
  
  retained_columns <- c(
    "Study",
    "T",
    "R",
    "N"
  )
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)),
               expected.label = format_vector_to_string(colnames(data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("ReorderColumns() reorders columns for binary wide data", {
  data <- CleanData(read.csv("data/Binary_wide.csv"))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Binary")
  
  retained_columns <- c(
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
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("ReorderColumns() retains covariate columns for long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  
  # Add covariate columns
  covariate_column_name_1 <- paste0(.covariate_prefix, "uno")
  covariate_column_name_2 <- paste0(.covariate_prefix, "zwei")
  covariate_column_name_3 <- paste0(.covariate_prefix, "trois")
  data[[covariate_column_name_1]] <- rep(1, nrow(data))
  data[[covariate_column_name_2]] <- rep(2, nrow(data))
  data[[covariate_column_name_3]] <- rep(3, nrow(data))
  
  # Add unused column
  data$deleteme <- rep("deleteme", nrow(data))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
  retained_columns <- c(
    "Study",
    "T",
    "N",
    "Mean",
    "SD",
    covariate_column_name_1,
    covariate_column_name_2,
    covariate_column_name_3
  )
  
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("ReorderColumns() retains covariate columns for wide data", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  
  # Add covariate columns
  covariate_column_name_1 <- paste0(.covariate_prefix, "uno")
  covariate_column_name_2 <- paste0(.covariate_prefix, "zwei")
  covariate_column_name_3 <- paste0(.covariate_prefix, "trois")
  data[[covariate_column_name_1]] <- rep(1, nrow(data))
  data[[covariate_column_name_2]] <- rep(2, nrow(data))
  data[[covariate_column_name_3]] <- rep(3, nrow(data))
  
  # Add unused column
  data$deleteme <- rep("deleteme", nrow(data))
  
  wrangled_data <- data %>%
    AddStudyIds() %>%
    ReorderColumns("Continuous")
  
  retained_columns <- c(
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
    "SD.3",
    covariate_column_name_1,
    covariate_column_name_2,
    covariate_column_name_3
  )
  
  expected_columns <- c("StudyID", retained_columns)
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  # Contents of columns unchanged
  for (col in retained_columns) {
    expect_equal(wrangled_data[[col]], data[[col]],
                 label = paste0("wrangled_data$", col),
                 expected.label = paste0("data$", col))
  }
})

test_that("SortByStudyIDThenT() sorts long data correctly", {
  long_data <- data.frame(StudyID = c(2, 2, 3, 3, 1, 1, 1),
                          Study = c("Cat", "Cat", "Dog", "Dog", "Cow", "Cow", "Cow"),
                          R = 1:7,
                          N = 11:17,
                          T = c(2, 1, 1, 3, 1, 2, 3))
  
  expected_sorted_data <- data.frame(StudyID = c(1, 1, 1, 2, 2, 3, 3),
                                     Study = c("Cow", "Cow", "Cow", "Cat", "Cat", "Dog", "Dog"),
                                     R = c(5, 6, 7, 2, 1, 3, 4),
                                     N = c(15, 16, 17, 12, 11, 13, 14),
                                     T = c(1, 2, 3, 1, 2, 1, 3))
  rownames(expected_sorted_data) <- as.integer(c(5, 6, 7, 2, 1, 3, 4))
  
  sorted_data <- SortByStudyIDThenT(data = long_data)
  
  expect_equal(expected_sorted_data, sorted_data)
})

test_that("SortByStudyIDThenT() sorts wide data correctly", {
  wide_data <- data.frame(StudyID = c(2, 3, 1),
                          Study = c("Cat", "Dog", "Cow"),
                          R.1 = c(1, 3, 5),
                          R.2 = c(2, 4, 6),
                          R.3 = c(NA, NA, 7),
                          N.1 = c(11, 13, 15),
                          N.2 = c(12, 14, 16),
                          N.3 = c(NA, NA, 17),
                          T.1 = c(2, 1, 1),
                          T.2 = c(1, 3, 2),
                          T.3 = c(NA, NA, 3))
  
  expected_sorted_data <- data.frame(StudyID = 1:3,
                                     Study = c("Cow", "Cat", "Dog"),
                                     T.1 = c(1, 1, 1),
                                     T.2 = c(2, 2, 3),
                                     T.3 = c(3, NA, NA),
                                     R.1 = c(5, 2, 3),
                                     R.2 = c(6, 1, 4),
                                     R.3 = c(7, NA, NA),
                                     N.1 = c(15, 12, 13),
                                     N.2 = c(16, 11, 14),
                                     N.3 = c(17, NA, NA))
  
  sorted_data <- SortByStudyIDThenT(data = wide_data, outcome_type = "Binary")
  
  expect_equal(expected_sorted_data, sorted_data)
})
  
test_that("WrangleUploadData() wrangles continuous long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Continuous")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "N", "Mean", "SD"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Order data to enable testing that the contents of columns have not changed
  data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  data$T <- c(1, 2, 3, 4, 1, 5, 1, 6)
  data <- data[order(data$StudyID, data$T), ]
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col == "T") {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("WrangleUploadData() wrangles continuous wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Continuous")

  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "T.2",
    "T.3",
    "N.1",
    "N.2",
    "N.3",
    "Mean.1",
    "Mean.2",
    "Mean.3",
    "SD.1",
    "SD.2",
    "SD.3"
  )

  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  ignored_columns <- paste0("T", c("", paste0(".", 1:6)))

  # Order data to enable testing that the contents of columns have not changed
  data$StudyID <- 1:3
  data$T.1 <- c(1, 2, 1)
  data$T.2 <- c(3, 1, 4)
  data$T.3 <- c(5, 6, NA)
  data <- data |> WideToLong("Continuous") |> SortLong() |> LongToWide("Continuous")
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col %in% ignored_columns) {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("WrangleUploadData() wrangles binary long data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/Binary_long.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  expect_equal(colnames(wrangled_data), c("StudyID", "Study", "T", "R", "N"),
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  # Order data to enable testing that the contents of columns have not changed
  data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  data$T <- c(1, 2, 3, 4, 1, 5, 1, 6)
  data <- data[order(data$StudyID, data$T), ]
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col == "T") {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("WrangleUploadData() wrangles binary wide data to be usable in the rest of the app", {
  data <- CleanData(read.csv("data/Binary_wide.csv"))
  treatment_ids <- data %>%
    FindAllTreatments() %>%
    CreateTreatmentIds()
  
  wrangled_data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  expected_columns <- c(
    "StudyID",
    "Study",
    "T.1",
    "T.2",
    "T.3",
    "R.1",
    "R.2",
    "R.3",
    "N.1",
    "N.2",
    "N.3"
  )
  
  expect_equal(colnames(wrangled_data), expected_columns,
               label = format_vector_to_string(colnames(wrangled_data)))
  
  expect_equal(wrangled_data$StudyID, c(1, 2, 3),
               label = format_vector_to_string(wrangled_data$StudyID))
  
  ignored_columns <- paste0("T", c("", paste0(".", 1:6)))
  
  # Order data to enable testing that the contents of columns have not changed
  data$StudyID <- 1:3
  data$T.1 <- c(1, 2, 1)
  data$T.2 <- c(3, 1, 4)
  data$T.3 <- c(5, 6, NA)
  data <- data |> WideToLong("Binary") |> SortLong() |> LongToWide("Binary")
  
  # Contents of columns unchanged
  for (col in colnames(data)) {
    if (col %in% ignored_columns) {
      next
    }
    expect_equal(!!wrangled_data[[col]], !!data[[col]])
  }
})

test_that("CleanTreatmentIds() does not change compliant treatment names", {
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("Flour", "Egg", "Sugar", "Butter", "Cinnamon"),
    RawLabel = c("Flour", "Egg", "Sugar", "Butter", "Cinnamon")
  )
  expect_equal(!!CleanTreatmentIds(treatment_ids ), !!treatment_ids)
})

test_that("CleanTreatmentIds() replaces spaces in treatment names", {
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("100g Flour", "1 Egg", "50g Sugar", "75g Butter", "2g Cinnamon")
  )
  
  expected_treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("100g_Flour", "1_Egg", "50g_Sugar", "75g_Butter", "2g_Cinnamon"),
    RawLabel = c("100g Flour", "1 Egg", "50g Sugar", "75g Butter", "2g Cinnamon")
  )
  expect_equal(!!CleanTreatmentIds(treatment_ids ), !!expected_treatment_ids)
})

test_that("CleanTreatmentIds() replaces special characters in treatment names", {
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("2*4=8", "lunch@8o'clock", "#R4Life", "I<3Shiny", ">o<")
  )
  
  expected_treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("2_4_8", "lunch_8o_clock", "_R4Life", "I_3Shiny", "_o_"),
    RawLabel = c("2*4=8", "lunch@8o'clock", "#R4Life", "I<3Shiny", ">o<")
  )
  expect_equal(!!CleanTreatmentIds(treatment_ids ), !!expected_treatment_ids)
})
 
test_that("CleanTreatmentIds() replaces multiple sequential special characters in treatment names with single underscore", {
  treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("2 * 4 = 8", "^(*(oo)*)^ <- It's a pig", "you stupid *%?$@#!", "var <- value", ":,-)")
  )

  expected_treatment_ids <- data.frame(
    Number = 1:5,
    Label = c("2_4_8", "_oo_It_s_a_pig", "you_stupid_", "var_value", "_"),
    RawLabel = c("2 * 4 = 8", "^(*(oo)*)^ <- It's a pig", "you stupid *%?$@#!", "var <- value", ":,-)")
  )
  expect_equal(!!CleanTreatmentIds(treatment_ids ), !!expected_treatment_ids)
})

test_that("CleanStudies() creates 'RawStudy' and modifies 'Study' as expected", {
  data <- data.frame(Study = c("Flour", "1 Egg", "@Sugar", "Cinn*%?$@#!amon"),
                     T = c("Treat1", "Treat2", "Treat1", "Treat2"))
  expected_cleaned_data <- data.frame(Study = c("Flour", "1_Egg", "_Sugar", "Cinn_amon"),
                                      T = c("Treat1", "Treat2", "Treat1", "Treat2"),
                                      RawStudy = c("Flour", "1 Egg", "@Sugar", "Cinn*%?$@#!amon"))
  cleaned_data <- CleanStudies(data)
  expect_equal(cleaned_data, expected_cleaned_data)
})

test_that("FindCovariateNames() finds covariate columns for long data", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  
  # Add covariate columns
  covariate_column_name_1 <- paste0(.covariate_prefix, "uno")
  covariate_column_name_2 <- paste0(.covariate_prefix, "zwei")
  covariate_column_name_3 <- paste0(.covariate_prefix, "trois")
  data[[covariate_column_name_1]] <- rep(1, nrow(data))
  data[[covariate_column_name_2]] <- rep(2, nrow(data))
  data[[covariate_column_name_3]] <- rep(3, nrow(data))
  
  # Add unused column
  data$deleteme <- rep("deleteme", nrow(data))
  
  expected_columns <- c(
    covariate_column_name_1,
    covariate_column_name_2,
    covariate_column_name_3
  )
  
  expect_equal(!!FindCovariateNames(data), expected_columns)
})

test_that("FindCovariateNames() finds covariate columns for wide data", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  
  # Add covariate columns
  covariate_column_name_1 <- paste0(.covariate_prefix, "uno")
  covariate_column_name_2 <- paste0(.covariate_prefix, "zwei")
  covariate_column_name_3 <- paste0(.covariate_prefix, "trois")
  data[[covariate_column_name_1]] <- rep(1, nrow(data))
  data[[covariate_column_name_2]] <- rep(2, nrow(data))
  data[[covariate_column_name_3]] <- rep(3, nrow(data))
  
  # Add unused column
  data$deleteme <- rep("deleteme", nrow(data))
  
  expected_columns <- c(
    covariate_column_name_1,
    covariate_column_name_2,
    covariate_column_name_3
  )
  
  expect_equal(!!FindCovariateNames(data), expected_columns)
})

test_that("GetFriendlyCovariateName() gets friendly covariate name", {
  base_name <- "Mohandas Karamchand Ghandi"
  covariate_column_name <- paste0(.covariate_prefix, base_name)
  
  expect_equal(!!GetFriendlyCovariateName(covariate_column_name), base_name)
})

test_that("RemoveCovariates() removes covariates for continuous long data", {
  data <- read.csv("data/Cont_long_continuous_cov.csv") %>%
    CleanData()
  
  column_names <- c(
    "Study",
    "T",
    "N",
    "Mean",
    "SD"
  )
  
  expect_equal(!!names(data), !!c(column_names, "covar.age"))
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() removes covariates for continuous wide data", {
  data <- read.csv("data/Cont_wide_continuous_cov.csv") %>%
    CleanData()
  
  column_names <- c(
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
  
  expect_equal(!!names(data), !!c(column_names, "covar.age"))
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() removes covariates for binary long data", {
  data <- read.csv("data/Binary_long_continuous_cov.csv") %>%
    CleanData()
  
  column_names <- c(
    "Study",
    "T",
    "R",
    "N"
  )
  
  expect_equal(!!names(data), !!c(column_names, "covar.age"))
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() removes covariates for binary wide data", {
  data <- read.csv("data/Binary_wide_continuous_cov.csv") %>%
    CleanData()
  
  column_names <- c(
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
  
  expect_equal(!!names(data), !!c(column_names, "covar.age"))
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() does nothing for continuous long data with no covariates", {
  data <- read.csv("data/Cont_long.csv") %>%
    CleanData()
  
  column_names <- c(
    "Study",
    "T",
    "N",
    "Mean",
    "SD"
  )
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() does nothing for continuous wide data with no covariates", {
  data <- read.csv("data/Cont_wide.csv") %>%
    CleanData()
  
  column_names <- c(
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
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() does nothing for binary long data with no covariates", {
  data <- read.csv("data/Binary_long.csv") %>%
    CleanData()
  
  column_names <- c(
    "Study",
    "T",
    "R",
    "N"
  )
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("RemoveCovariates() does nothing for binary wide data with no covariates", {
  data <- read.csv("data/Binary_wide.csv") %>%
    CleanData()
  
  column_names <- c(
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
  
  data <- data %>%
    RemoveCovariates()
  
  expect_equal(!!names(data), !!column_names)
})

test_that("FindDataShape() finds shape of continuous long data", {
  data <- read.csv("data/Cont_long_continuous_cov.csv") %>%
    CleanData()
  
  expect_equal(!!FindDataShape(data), "long")
})

test_that("FindDataShape() finds shape of continuous wide data", {
  data <- read.csv("data/Cont_wide_continuous_cov.csv") %>%
    CleanData()
  
  expect_equal(!!FindDataShape(data), "wide")
})

test_that("FindDataShape() finds shape of binary long data", {
  data <- read.csv("data/Binary_long_continuous_cov.csv") %>%
    CleanData()
  
  expect_equal(!!FindDataShape(data), "long")
})

test_that("FindDataShape() finds shape of binary wide data", {
  data <- read.csv("data/Binary_wide_continuous_cov.csv") %>%
    CleanData()
  
  expect_equal(!!FindDataShape(data), "wide")
})

test_that("WideToLong() correctly converts binary wide data with binary covariates", {
  wide_data <- read.csv("data/Binary_wide_binary_cov.csv")
  long_data <- WideToLong(wide_data, "Binary")
  expected_data <- read.csv("data/Binary_long_binary_cov.csv")
  
  expect_equal(long_data, expected_data)
})

test_that("WideToLong() correctly converts binary wide data with continuous covariates", {
  wide_data <- read.csv("data/Binary_wide_continuous_cov.csv")
  long_data <- WideToLong(wide_data, "Binary")
  expected_data <- read.csv("data/Binary_long_continuous_cov.csv")
  
  expect_equal(long_data, expected_data)
})

test_that("WideToLong() correctly converts continuous wide data with binary covariates", {
  wide_data <- read.csv("data/Cont_wide_binary_cov.csv")
  long_data <- WideToLong(wide_data, "Continuous")
  expected_data <- read.csv("data/Cont_long_binary_cov.csv")
  
  expect_equal(long_data, expected_data)
})

test_that("WideToLong() correctly converts continuous wide data with continuous covariates", {
  wide_data <- read.csv("data/Cont_wide_continuous_cov.csv")
  long_data <- WideToLong(wide_data, "Continuous")
  expected_data <- read.csv("data/Cont_long_continuous_cov.csv")
  
  expect_equal(long_data, expected_data)
})

test_that("LongToWide() correctly converts binary wide data with binary covariates", {
  long_data <- read.csv("data/Binary_long_binary_cov.csv")
  long_data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  wide_data <- LongToWide(long_data, "Binary") %>% 
    dplyr::relocate(sort(names(.)))
  expected_data <- read.csv("data/Binary_wide_binary_cov.csv")
  expected_data$StudyID <- 1:3
  expected_data <- expected_data %>% 
    dplyr::relocate(sort(names(.)))
  
  expect_equal(wide_data, expected_data)
})

test_that("LongToWide() correctly converts binary wide data with continuous covariates", {
  long_data <- read.csv("data/Binary_long_continuous_cov.csv")
  long_data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  wide_data <- LongToWide(long_data, "Binary") %>% 
    dplyr::relocate(sort(names(.)))
  expected_data <- read.csv("data/Binary_wide_continuous_cov.csv")
  expected_data$StudyID <- 1:3
  expected_data <- expected_data %>% 
    dplyr::relocate(sort(names(.)))
  
  expect_equal(wide_data, expected_data)
})

test_that("LongToWide() correctly converts continuous wide data with binary covariates", {
  long_data <- read.csv("data/Cont_long_binary_cov.csv")
  long_data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  wide_data <- LongToWide(long_data, "Continuous") %>% 
    dplyr::relocate(sort(names(.)))
  expected_data <- read.csv("data/Cont_wide_binary_cov.csv")
  expected_data$StudyID <- 1:3
  expected_data <- expected_data %>% 
    dplyr::relocate(sort(names(.)))
  
  expect_equal(wide_data, expected_data)
})

test_that("LongToWide() correctly converts continuous wide data with continuous covariates", {
  long_data <- read.csv("data/Cont_long_continuous_cov.csv")
  long_data$StudyID <- c(1, 1, 1, 2, 2, 2, 3, 3)
  wide_data <- LongToWide(long_data, "Continuous") %>% 
    dplyr::relocate(sort(names(.)))
  expected_data <- read.csv("data/Cont_wide_continuous_cov.csv")
  expected_data$StudyID <- 1:3
  expected_data <- expected_data %>% 
    dplyr::relocate(sort(names(.)))
  
  expect_equal(wide_data, expected_data)
})


test_that("KeepOrDeleteControlTreatment() deletes only rows with control treatments", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"))
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  data_no_control <- data.frame(Study = c("A", "B", "C", "C", "D"),
                                T = c(2, 3, 2, 3, 4),
                                Treatment = c("Oxygen", "Sulphur", "Oxygen", "Sulphur", "Zinc"),
                                Control = c("Hydrogen", "Oxygen", "Hydrogen", "Hydrogen", "Sulphur"))
  attr(data_no_control, "row.names") <- as.integer(c(2, 4, 6, 7, 9))
  
  expect_equal(KeepOrDeleteControlTreatment(data, treatments, "delete"), data_no_control)
})


test_that("KeepOrDeleteControlTreatment() keeps only rows with control treatments", {
  data <- data.frame(Study = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
                     T = c(1, 2, 2, 3, 1, 2, 3, 3, 4),
                     Treatment = c("Hydrogen", "Oxygen", "Oxygen", "Sulphur", "Hydrogen", "Oxygen", "Sulphur", "Sulphur", "Zinc"))
  
  treatments <- c("Hydrogen", "Oxygen", "Sulphur", "Zinc")
  
  data_control <- data.frame(Study = c("A", "B", "C", "D"),
                             T = c(1, 2, 1, 3),
                             Treatment = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"),
                             Control = c("Hydrogen", "Oxygen", "Hydrogen", "Sulphur"))
  attr(data_control, "row.names") <- as.integer(c(1, 3, 5, 8))
  
  expect_equal(KeepOrDeleteControlTreatment(data, treatments, "keep"), data_control)
})


test_that("CreateListOfWideColumns() creates the correct list", {
  data <- read.csv("data\\Cont_wide.csv")
  expected_list_T <- list(T.1 = data$T.1, T.2 = data$T.2, T.3 = data$T.3)
  expected_list_Mean <- list(Mean.1 = data$Mean.1, Mean.2 = data$Mean.2, Mean.3 = data$Mean.3)
  expect_equal(CreateListOfWideColumns(data, "T"), expected_list_T)
  expect_equal(CreateListOfWideColumns(data, "Mean"), expected_list_Mean)
})


test_that("FindMaxArms() returns the correct number of arms for long data", {
  data <- read.csv("data\\Cont_long.csv")
  expect_equal(FindMaxArms(data), 3)
})


test_that("FindMaxArms() returns the correct number of arms for wide data", {
  data <- read.csv("data\\Cont_wide.csv")
  expect_equal(FindMaxArms(data), 3)
})


test_that("RoundForDisplay() rounds correctly", {
  expect_equal(RoundForDisplay(1234.5678), 1234.6)
  expect_equal(RoundForDisplay(0.000789), 0.0008)
})
