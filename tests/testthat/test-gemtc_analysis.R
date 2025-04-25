test_that("PrepDataGemtc() gives correct data for wide binary", {

  # process data as would be in app
  data <- read.csv("data/Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  gemtc_data <- PrepDataGemtc(data, wrangled_treatment_list, "Binary", "covar.age", "age")

  expected_armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = c(30, 31, 32, 34, 33, 35, 36, 37),
    sampleSize = c(100, 101, 102, 104, 103, 105, 106, 107))
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))

  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for long binary", {

  data <- read.csv("data/Binary_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  gemtc_data <- PrepDataGemtc(data, wrangled_treatment_list, "Binary", "covar.age", "age")

  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = c(30, 31, 32, 34, 33, 35, 36, 37),
    sampleSize = c(100, 101, 102, 104, 103, 105, 106, 107))
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))

  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for wide continuous", {

  data <- read.csv("data/Cont_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  gemtc_data <- PrepDataGemtc(data, wrangled_treatment_list, "Continuous", "covar.age", "age")

  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    mean = c(-1, -1.1, -1.2, -1.4, -1.3, -1.5, -1.6, -1.7),
    std.dev = c(11.1, 12.2, 13.3, 15.5, 14.4, 16.6, 17.7, 18.8),
    sampleSize = c(30, 31, 32, 34, 33, 35, 36, 37))
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))

  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("PrepDataGemtc() gives correct data for long continuous", {

  data <- read.csv("data/Cont_long_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data))
  data <- WrangleUploadData(data, treatment_ids, "Continuous")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)

  gemtc_data <- PrepDataGemtc(data, wrangled_treatment_list, "Continuous", "covar.age", "age")

  expected_armData <- data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Great", "the_Little", "the_Butcher", "the_Great", "the_Slit_nosed"),
    mean = c(-1, -1.1, -1.2, -1.4, -1.3, -1.5, -1.6, -1.7),
    std.dev = c(11.1, 12.2, 13.3, 15.5, 14.4, 16.6, 17.7, 18.8),
    sampleSize = c(30, 31, 32, 34, 33, 35, 36, 37))
  expected_studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))

  expect_equal(gemtc_data$armData, expected_armData)
  expect_equal(gemtc_data$studyData, expected_studyData)
})

test_that("CreateGemtcModel() assigns model type, covariate type, reference treatment, and RNGs correctly", {

  data <- list(armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = 30:37,
    sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'OR', 'unrelated', 'the_Great')

  expect_equal(gemtc_model$linearModel, "random")
  expect_equal(gemtc_model$regressor$coefficient, "unrelated")
  expect_equal(as.character(gemtc_model$regressor$control), "the_Great")
  expect_equal(gemtc_model$inits[[1]]$.RNG.name, "base::Wichmann-Hill")
  expect_equal(gemtc_model$inits[[2]]$.RNG.name, "base::Marsaglia-Multicarry")
  expect_equal(gemtc_model$inits[[3]]$.RNG.name, "base::Super-Duper")
  expect_equal(gemtc_model$inits[[4]]$.RNG.name, "base::Mersenne-Twister")

})

test_that("CreateGemtcModel() has correct model settings for OR outcome", {

  data <- list(armData = data.frame(
      study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      responders = 30:37,
      sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'OR', 'shared', 'the_Great')

  expect_equal(gemtc_model$likelihood, "binom")
  expect_equal(gemtc_model$link, "logit")

})

test_that("CreateGemtcModel() has correct model settings for RR outcome", {

  data <- list(armData = data.frame(
    study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
    treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
    responders = 30:37,
    sampleSize = 100:107),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'RR', 'shared', 'the_Great')

  expect_equal(gemtc_model$likelihood, "binom")
  expect_equal(gemtc_model$link, "log")

})

test_that("CreateGemtcModel() has correct model settings for MD outcome", {

  data <- list(armData = data.frame(
      study = c(rep("Constantine", 3), rep("Leo", 3), rep("Justinian", 2)),
      treatment = c("the_Great", "the_Younger", "the_Dung_named", "the_Little", "the_Great", "the_Butcher", "the_Great", "the_Slit_nosed"),
      mean = c(-1, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
      std.dev = c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
      sampleSize = 30:37),
    studyData = data.frame(
      study = c("Constantine", "Leo", "Justinian"),
      age = c(99, 98, 97))
  )
  gemtc_model <- CreateGemtcModel(data, 'random', 'MD', 'shared', 'the_Great')

  expect_equal(gemtc_model$likelihood, "normal")
  expect_equal(gemtc_model$link, "identity")

})

test_that("RunCovariateModel() gives reproducible output.
          FindCovariateDefault() gives correct output.
          CovariateModelOutput() gives correct output.
          GetGemtcMcmcCharacteristics() returns correct MCMC data.", {
  reference = "the_Little"

  data <- read.csv("data/Binary_wide_continuous_cov.csv")
  treatment_ids <- CreateTreatmentIds(FindAllTreatments(data), reference_treatment = reference)
  data <- WrangleUploadData(data, treatment_ids, "Binary")
  wrangled_treatment_list <- CleanTreatmentIds(treatment_ids)
  outcome_measure = "OR"

  result_1 <- RunCovariateModel(data, wrangled_treatment_list, "Binary", outcome_measure, "covar.age", "age", 'random', 'unrelated', reference)
  result_2 <- RunCovariateModel(data, wrangled_treatment_list, "Binary", outcome_measure, "covar.age", "age", 'random', 'unrelated', reference)

  expect_equal(result_1$samples[1], result_2$samples[1])
  expect_equal(result_1$samples[2], result_2$samples[2])
  expect_equal(result_1$samples[3], result_2$samples[3])
  expect_equal(result_1$samples[4], result_2$samples[4])

  default <- FindCovariateDefault(result_1)

  covariate_value = 98

  expect_equal(default, covariate_value)

  output_1 <- CovariateModelOutput(
    data = data,
    treatment_ids = wrangled_treatment_list,
    model = result_1,
    covariate_title = "covar.age",
    cov_value = default,
    outcome_measure = outcome_measure,
    covariate_type = "Continuous"
  )

  expect_equal(length(output_1), 17)

  expect_equal(output_1$a, "random effect")
  expect_equal(output_1$cov_value_sentence, "Value for covariate age set at 98")
  expect_equal(output_1$outcome, outcome_measure)
  expect_equal(output_1$model, "random")
  expect_equal(output_1$covariate_value, covariate_value)
  expect_equal(output_1$reference_name, reference)
  expect_equal(output_1$comparator_names, c("the_Butcher", "the_Dung_named", "the_Great", "the_Slit_nosed", "the_Younger"))
  
  expected_mcmc_table <- data.frame(characteristic = c("Chains",
                                                       "Burn-in iterations",
                                                       "Sample iterations",
                                                       "Thinning factor"),
                                    value = c(4, 5000, 20000, 1))
  
  expect_equal(GetGemtcMcmcCharacteristics(result_1), expected_mcmc_table)
})

test_that("CalculateCredibleRegions() gives nothing for NA evidence range", {
  mtc_results <- list()
  
  model_output <- list(
    mtcResults = mtc_results,
    reference_name = "Placebo",
    comparator_names = c("Ibuprofen"),
    covariate_min = c(Ibuprofen = NA),
    covariate_max = c(Ibuprofen = NA)
  )
  
  result <- CalculateCredibleRegions(model_output)
  
  expect_equal(names(result$intervals), c("Ibuprofen"))
  expect_equal(names(result$regions), c("Ibuprofen"))
  
  expect_equal(
    result$intervals["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(cov_value = NA, lower = NA, upper = NA)
    )
  )
  expect_equal(
    result$regions["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(cov_value = NA, lower = NA, upper = NA)
    )
  )
})

test_that("CalculateCredibleRegions() gives interval for zero-width evidence range", {
  mtc_results <- list()
  
  model_output <- list(
    mtcResults = mtc_results,
    reference_name = "Placebo",
    comparator_names = c("Ibuprofen"),
    covariate_min = c(Ibuprofen = 7),
    covariate_max = c(Ibuprofen = 7)
  )
  
  interval <- c("2.5%" = 11, "97.5%" = 11)
  
  rel_eff_summary <- list(
    summaries = list(
      quantiles = interval
    )
  )
  
  mockery::stub(CalculateCredibleRegions, ".FindCredibleInterval", interval)
  
  result <- CalculateCredibleRegions(model_output)
  
  expect_equal(names(result$regions), c("Ibuprofen"))
  expect_equal(names(result$intervals), c("Ibuprofen"))
  
  expect_equal(
    result$intervals["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(cov_value = 7, lower = 11, upper = 11)
    )
  )
  expect_equal(
    result$regions["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(cov_value = NA, lower = NA, upper = NA)
    )
  )
})

test_that("CalculateCredibleRegions() gives region for non-zero-width evidence range", {
  mtc_results <- list()
  mtc_results$model <- list()
  mtc_results$model$regressor <- list(type="continuous")
  
  model_output <- list(
    mtcResults = mtc_results,
    reference_name = "Placebo",
    comparator_names = c("Ibuprofen"),
    covariate_min = c(Ibuprofen = 7),
    covariate_max = c(Ibuprofen = 17)
  )
  
  interval <- matrix(
    data = c(
      11, 12,
      12, 13,
      13, 14,
      14, 15,
      15, 16,
      16, 17,
      17, 18,
      18, 19,
      19, 20,
      20, 21,
      21, 22
    ),
    nrow = 11,
    ncol = 2,
    dimnames = list(
      NULL,
      c("2.5%", "97.5%")
    ),
    byrow = TRUE
  )
  
  rel_eff_summary <- list(
    summaries = list(
      quantiles = interval
    )
  )
  
  n <- 1
  mockery::stub(
    CalculateCredibleRegions,
    ".FindCredibleInterval",
    function(...) {
      this_interval <- interval[n, c("2.5%", "97.5%")]
      n <<- n + 1
      return(this_interval)
    }
  )
  
  result <- CalculateCredibleRegions(model_output)
  
  expect_equal(names(result$intervals), c("Ibuprofen"))
  expect_equal(names(result$regions), c("Ibuprofen"))
  
  expect_equal(
    result$regions["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(
        cov_value = c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
        lower = c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
        upper = c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
      )
    )
  )
  expect_equal(
    result$intervals["Ibuprofen"],
    list(
      "Ibuprofen" = data.frame(cov_value = NA, lower = NA, upper = NA)
    )
  )
})
