
LoadCinemaData <- function(file_path = "data/cinema_data/NMA_data_binary_FE_two_arm_CINeMA.csv") {
  data <- CleanData(read.csv(file_path))
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
  reference = cinema_data$treatment_ids$Label[cinema_data$treatment_ids$Number == 1]
  wide_data <- LongToWide(long_data = cinema_data$long_data, outcome_type = cinema_data$outcome_type)
  
  # transform data to contrast form
  d0 <- contrastform.df(wide_data, cinema_data$treatment_ids, outcome_measure, cinema_data$outcome_type)
  #obtain treatment labels
  lstx <- cinema_data$treatment_ids$Label
  #count treatment numbers
  ntx <- length(lstx)
  #matching treatment labels to treatment code
  d1 <- labelmatching.df(d1 = d0, ntx = ntx, treat_list = cinema_data$treatment_ids)
  # NMA of all studies
  net1 <- freq.df(model = model_type, outcome = outcome_measure, dataf = d1, ref = reference)
  
  freq_all <- list(
    net1 = net1,
    lstx = lstx,
    ntx = ntx,
    d0 = d0,
    d1 = d1
  )
  
  contributions <- netmeta::netcontrib(
    x = freq_all$net1,
    method = "shortestpath"
  )
  
  return(contributions)
}


cinema_data <- LoadCinemaData()
model_type <- "fixed"
outcome_measure <- "OR"
cinema_analysis <- GenerateCinemaAnalysis(cinema_data, model_type, outcome_measure)


test_that("Should produce valid JSON", {
  prepped_project <- PrepareProjectForCinema(cinema_data$long_data, cinema_data$treatment_ids, cinema_data$outcome_type, cinema_analysis, model_type, outcome_measure)
  
  json <- jsonlite::toJSON(prepped_project, pretty = TRUE)
  
  result <- jsonvalidate::json_validate(json, "../../cinema/cinema_schema.json", verbose = TRUE)
  
  expect_true(result, label = result)
})