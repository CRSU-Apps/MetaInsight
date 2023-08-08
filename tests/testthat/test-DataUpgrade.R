
test_that("CreateTreatmentsDataFrame() parses perfect treatment string", {
  treatments_string <- "Egg,Flour,Sugar,Butter,Cinnamon"
  
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  expect_equal(colnames(treatment_df), c("Number", "Label"),
               label = format_vector_to_string(colnames(treatment_df)))
  expect_equal(nrow(treatment_df), 5,
               label = nrow(treatment_df))
  expect_equal(treatment_df$Number, 1:5,
               label = format_vector_to_string(treatment_df$Number))
  expect_equal(treatment_df$Label, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"),
               label = format_vector_to_string(treatment_df$Label))
})

test_that("CreateTreatmentsDataFrame() parses string with internal, leading and trailing spaces", {
  treatments_string <- " Egg , Flour , Sugar ,  Butter , Cinnamon  "
  
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  expect_equal(colnames(treatment_df), c("Number", "Label"),
               label = format_vector_to_string(colnames(treatment_df)))
  expect_equal(nrow(treatment_df), 5,
               label = nrow(treatment_df))
  expect_equal(treatment_df$Number, 1:5,
               label = format_vector_to_string(treatment_df$Number))
  expect_equal(treatment_df$Label, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"),
               label = format_vector_to_string(treatment_df$Label))
})

test_that("CreateTreatmentsDataFrame() parses string with duplicated and trailing commas", {
  treatments_string <- "Egg,,Flour,Sugar,Butter,Cinnamon,"
  
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  expect_equal(colnames(treatment_df), c("Number", "Label"),
               label = format_vector_to_string(colnames(treatment_df)))
  expect_equal(nrow(treatment_df), 5,
               label = nrow(treatment_df))
  expect_equal(treatment_df$Number, 1:5,
               label = format_vector_to_string(treatment_df$Number))
  expect_equal(treatment_df$Label, c("Egg", "Flour", "Sugar", "Butter", "Cinnamon"),
               label = format_vector_to_string(treatment_df$Label))
})

test_that("UpgradeData() upgrades long-format data", {
  old_data <- data.frame(
    StudyID = c(1, 1, 2, 2, 3, 3, 3),
    Study = c("A", "A", "B", "B", "C", "C", "C"),
    T = c(1, 2, 1, 3, 1, 4, 5),
    OtherText = c("A", "A", "B", "B", "C", "C", "C")
  )
  treatments_string <- "Egg,Flour,Sugar,Butter,Cinnamon"
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  upgraded_data <- UpgradeData(old_data, treatment_df)
  
  expect_equal(colnames(upgraded_data), c("Study", "T", "OtherText"),
               label = format_vector_to_string(colnames(upgraded_data)))
  expect_equal(nrow(upgraded_data), 7,
               label = nrow(upgraded_data))
  expect_equal(upgraded_data$Study, c("A", "A", "B", "B", "C", "C", "C"),
               label = format_vector_to_string(upgraded_data$Study))
  expect_equal(upgraded_data$T, c("Egg", "Flour", "Egg", "Sugar", "Egg", "Butter", "Cinnamon"),
               label = format_vector_to_string(upgraded_data$T))
  expect_equal(upgraded_data$OtherText, c("A", "A", "B", "B", "C", "C", "C"),
               label = format_vector_to_string(upgraded_data$OtherText))
})

test_that("UpgradeData() upgrades long-format data with unnecessary spaces", {
  old_data <- data.frame(
    StudyID = c(1, 1, 2, 2, 3, 3, 3),
    Study = c("A ", "A ", " B ", " B ", "C", "C", "C"),
    T = c(1, 2, 1, 3, 1, 4, 5),
    OtherText = c("A ", "A ", " B ", " B ", "C", "C", "C")
  )
  treatments_string <- "Egg,Flour,Sugar,Butter,Cinnamon"
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  upgraded_data <- UpgradeData(old_data, treatment_df)
  
  expect_equal(colnames(upgraded_data), c("Study", "T", "OtherText"),
               label = format_vector_to_string(colnames(upgraded_data)))
  expect_equal(nrow(upgraded_data), 7,
               label = nrow(upgraded_data))
  expect_equal(upgraded_data$Study, c("A", "A", "B", "B", "C", "C", "C"),
               label = format_vector_to_string(upgraded_data$Study))
  expect_equal(upgraded_data$T, c("Egg", "Flour", "Egg", "Sugar", "Egg", "Butter", "Cinnamon"),
               label = format_vector_to_string(upgraded_data$T))
  expect_equal(upgraded_data$OtherText, c("A", "A", "B", "B", "C", "C", "C"),
               label = format_vector_to_string(upgraded_data$OtherText))
})

test_that("UpgradeData() upgrades wide-format data", {
  old_data <- data.frame(
    StudyID = c(1, 2, 3),
    Study = c("A", "B", "C"),
    T.1 = c(1, 1, 1),
    OtherText.1 = c("A", "B", "C"),
    T.2 = c(2, 3, 4),
    OtherText.2 = c("A", "B", "C"),
    T.3 = c(NA, NA, 5),
    OtherText.3 = c(NA, NA, "C")
  )
  treatments_string <- "Egg,Flour,Sugar,Butter,Cinnamon"
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  upgraded_data <- UpgradeData(old_data, treatment_df)
  
  expect_equal(colnames(upgraded_data), c("Study", "T.1", "OtherText.1", "T.2", "OtherText.2", "T.3", "OtherText.3"),
               label = format_vector_to_string(colnames(upgraded_data)))
  expect_equal(nrow(upgraded_data), 3,
               label = nrow(upgraded_data))
  expect_equal(upgraded_data$Study, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$Study))
  expect_equal(upgraded_data$T.1, c("Egg", "Egg", "Egg"),
               label = format_vector_to_string(upgraded_data$T.1))
  expect_equal(upgraded_data$OtherText.1, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$OtherText.1))
  expect_equal(upgraded_data$T.2, c("Flour", "Sugar", "Butter"),
               label = format_vector_to_string(upgraded_data$T.2))
  expect_equal(upgraded_data$OtherText.2, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$OtherText.2))
  expect_equal(upgraded_data$T.3, c(NA, NA, "Cinnamon"),
               label = format_vector_to_string(upgraded_data$T.3))
  expect_equal(upgraded_data$OtherText.3, c(NA, NA, "C"),
               label = format_vector_to_string(upgraded_data$OtherText.3))
})

test_that("UpgradeData() upgrades wide-format data with unnecessary spaces", {
  old_data <- data.frame(
    StudyID = c(1, 2, 3),
    Study = c("A ", " B ", "C"),
    T.1 = c(1, 1, 1),
    OtherText.1 = c(" A", " B ", "C"),
    T.2 = c(2, 3, 4),
    OtherText.2 = c(" A", " B ", "C"),
    T.3 = c(NA, NA, 5),
    OtherText.3 = c(NA, NA, "C")
  )
  treatments_string <- "Egg,Flour,Sugar,Butter,Cinnamon"
  treatment_df <- CreateTreatmentsDataFrame(treatments_string)
  
  upgraded_data <- UpgradeData(old_data, treatment_df)
  
  expect_equal(colnames(upgraded_data), c("Study", "T.1", "OtherText.1", "T.2", "OtherText.2", "T.3", "OtherText.3"),
               label = format_vector_to_string(colnames(upgraded_data)))
  expect_equal(nrow(upgraded_data), 3,
               label = nrow(upgraded_data))
  expect_equal(upgraded_data$Study, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$Study))
  expect_equal(upgraded_data$T.1, c("Egg", "Egg", "Egg"),
               label = format_vector_to_string(upgraded_data$T.1))
  expect_equal(upgraded_data$OtherText.1, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$OtherText.1))
  expect_equal(upgraded_data$T.2, c("Flour", "Sugar", "Butter"),
               label = format_vector_to_string(upgraded_data$T.2))
  expect_equal(upgraded_data$OtherText.2, c("A", "B", "C"),
               label = format_vector_to_string(upgraded_data$OtherText.2))
  expect_equal(upgraded_data$T.3, c(NA, NA, "Cinnamon"),
               label = format_vector_to_string(upgraded_data$T.3))
  expect_equal(upgraded_data$OtherText.3, c(NA, NA, "C"),
               label = format_vector_to_string(upgraded_data$OtherText.3))
})
