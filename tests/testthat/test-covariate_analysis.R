
test_that("FindCovariateRanges() finds ranges for continuous long data", {
  data <- CleanData(read.csv("Contribution_continuous_long_continuous_cov.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
  ranges <- FindCovariateRanges(
    data = wrangled_data,
    treatment_ids = treatment_ids,
    reference = "Paracetamol",
    covariate_title = "covar.age"
  )
  
  expected_min = c(
    "Ibuprofen" = 98,
    "A stiff drink" = 95,
    "Sleep" = 97,
    "Exercise" = NA
  )
  
  expected_max = c(
    "Ibuprofen" = 99,
    "A stiff drink" = 99,
    "Sleep" = 98,
    "Exercise" = NA
  )
  
  expect_mapequal(!!expected_min, !!ranges$min)
  expect_mapequal(!!expected_max, !!ranges$max)
})

test_that("FindCovariateRanges() finds ranges for continuous wide data", {
  data <- CleanData(read.csv("Contribution_continuous_wide_continuous_cov.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  
  ranges <- FindCovariateRanges(
    data = wrangled_data,
    treatment_ids = treatment_ids,
    reference = "Paracetamol",
    covariate_title = "covar.age"
  )
  
  expected_min = c(
    "Ibuprofen" = 98,
    "A stiff drink" = 95,
    "Sleep" = 97,
    "Exercise" = NA
  )
  
  expected_max = c(
    "Ibuprofen" = 99,
    "A stiff drink" = 99,
    "Sleep" = 98,
    "Exercise" = NA
  )
  
  expect_mapequal(!!expected_min, !!ranges$min)
  expect_mapequal(!!expected_max, !!ranges$max)
})
