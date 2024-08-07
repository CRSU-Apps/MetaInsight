
test_that("Covariate value taken from numeric input when continuous", {
  df <- read.csv("data/Cont_long_continuous_cov.csv") %>%
    CleanData()
  testServer(
    covariate_value_panel_server,
    args = list(
      covariate_type = reactive({ "Continuous" }),
      covariate_data = reactive({ df$covar.age })
    ),
    {
      session$setInputs(numeric = 14)
      session$flushReact()
      
      session$setInputs(toggle = TRUE)
      session$flushReact()
      
      expect_equal(covariate_value(), 14)
    }
  )
})

test_that("Covariate value taken from numeric input when binary", {
  df <- read.csv("data/Cont_long_binary_cov.csv") %>%
    CleanData()
  testServer(
    covariate_value_panel_server,
    args = list(
      covariate_type = reactive({ "Binary" }),
      covariate_data = reactive({ df$covar.handedness })
    ),
    {
      session$setInputs(toggle = TRUE)
      session$flushReact()
      
      session$setInputs(numeric = 14)
      session$flushReact()
      
      expect_equal(covariate_value(), 1)
    }
  )
})
