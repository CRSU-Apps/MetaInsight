
test_that("Covariate title NA when not available", {
  df <- read.csv("data/Cont_long.csv") %>%
    CleanData()
  testServer(
    covariate_analysis_panel_server,
    args = list(all_data = reactive({ df })),
    {
      expect_true(is.na(covariate_title()))
    }
  )
})

test_that("Covariate title extracted from data when available", {
  df <- read.csv("data/Cont_long_continuous_cov.csv") %>%
    CleanData()
  testServer(
    covariate_analysis_panel_server,
    args = list(all_data = reactive({ df })),
    {
      expect_equal(covariate_title(), "covar.age")
    }
  )
})
test_that("Covariate name NA when not available", {
  df <- read.csv("data/Cont_long.csv") %>%
    CleanData()
  testServer(
    covariate_analysis_panel_server,
    args = list(all_data = reactive({ df })),
    {
      expect_true(is.na(covariate_name()))
    }
  )
})

test_that("Covariate name extracted from data when available", {
  df <- read.csv("data/Cont_long_continuous_cov.csv") %>%
    CleanData()
  
  testServer(
    covariate_analysis_panel_server,
    args = list(all_data = reactive({ df }),
                treatment_df = reactive(NULL),
                reference_treatment = reactive(NULL),
                metaoutcome = reactive("Continuous"),
                outcome_measure = reactive("MD"),
                model_effects = reactive("random"),
                bugsnetdt = reactive(NULL)),
    {
      expect_equal(covariate_name(), "age")
      expect_equal(output$subtitle, "Covariate: age")
    }
  )
})
