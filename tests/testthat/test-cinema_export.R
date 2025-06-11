
LoadCinemaData <- function() {
  data <- CleanData(read.csv("data/cinema_data/NMA_data_binary_FE_two_arm_CINeMA.csv"))
  outcome_type = "Binary"
  all_treatments <- FindAllTreatments(data)
  treatment_ids <- CreateTreatmentIds(all_treatments)
  data <- WrangleUploadData(data, treatment_ids, outcome_type)
  
  return(
    list(
      long_data = data,
      treatment_ids = treatment_ids,
      outcome_type = outcome_type
    )
  )
}

GenerateCinemaAnalysis <- function(cinema_data, model_type, outcome_measure) {
  pairwise1 <- meta::pairwise(
    treat = T,
    event = R,
    n = N,
    studlab = Study,
    data = cinema_data$long_data,
    sm = outcome_measure
  )
  
  netmeta1 <- netmeta::netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = Study,
    common = model_type == "fixed",
    random = model_type == "random",
    data = pairwise1,
    sm = outcome_measure
  )
  
  contributions <- netmeta::netcontrib(
    x = netmeta1,
    method = "shortestpath"
  )
  
  return(contributions)
}


cinema_data <- LoadCinemaData()
model_type <- "fixed"
outcome_measure <- "OR"
cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)


test_that("Should identify incorrect types", {
  prepped_project <- PrepareProjectForCinema(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type, cinema_analysis, model_type, outcome_measure)
  
  json <- jsonlite::toJSON(prepped_project, pretty = TRUE)
  
  result <- jsonvalidate::json_validate(json, "../../cinema/cinema_schema.json", verbose = TRUE)
  
  expect_true(result, label = result)
})