
test_that("FindAllStudiesBetweenTreatments() finds ranges for continuous long data", {
  data <- CleanData(read.csv("Contribution_continuous_long_continuous_cov.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  reference = "Paracetamol"
  
  links <- FindAllStudiesBetweenTreatments(data = wrangled_data, treatment_df = treatment_ids, reference_treatment_name = reference)
  
  studies <- unique(data$Study)
  
  expected_links = matrix(
    data = c(
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      FALSE, FALSE, FALSE, TRUE
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE,
    dimnames = list(
      studies,
      treatment_ids$Label[treatment_ids$Label != reference]
    )
  )
  
  expect_equal(!!expected_links, !!links)
})

test_that("FindAllStudiesBetweenTreatments() finds ranges for continuous wide data", {
  data <- CleanData(read.csv("Contribution_continuous_wide_continuous_cov.csv"))
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments, all_treatments[1])
  
  wrangled_data <- ReplaceTreatmentIds(data, treatment_ids)
  reference = "Paracetamol"
  
  links <- FindAllStudiesBetweenTreatments(data = wrangled_data, treatment_df = treatment_ids, reference_treatment_name = reference)
  
  studies <- unique(data$Study)
  
  expected_links = matrix(
    data = c(
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      TRUE,  TRUE,  TRUE,  TRUE,
      FALSE, FALSE, FALSE, TRUE
    ),
    nrow = length(studies),
    ncol = length(treatment_ids$Label) - 1,
    byrow = TRUE,
    dimnames = list(
      studies,
      treatment_ids$Label[treatment_ids$Label != reference]
    )
  )
  
  expect_equal(!!expected_links, !!links)
})
