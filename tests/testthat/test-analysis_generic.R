test_that(".FindTreatmentCount() finds number of treatment columns for binary data", {
  
  # process data as would be in app
  data <- read.csv("Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  expect_equal(.FindTreatmentCount(data), 3)
})

test_that(".FindTreatmentCount() finds number of treatment columns for continuous data", {
  
  # process data as would be in app
  data <- read.csv("Cont_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  
  expect_equal(.FindTreatmentCount(data), 3)
})

test_that(".FindVaryingColumnIndices() finds all varying columns for binary data", {
  
  # process data as would be in app
  data <- read.csv("Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  
  expect_equal(
    !!.FindVaryingColumnIndices(data),
    c(3, 4, 5, 6, 7, 8, 9, 10, 11)
  )
})

test_that(".FindVaryingColumnIndices() finds all varying columns for continuous data", {
  
  # process data as would be in app
  data <- read.csv("Cont_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  print(data)
  
  expect_equal(
    !!.FindVaryingColumnIndices(data),
    c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  )
})
