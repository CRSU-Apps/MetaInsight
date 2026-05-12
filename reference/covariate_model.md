# Fit a covariate regression model

Fit a covariate regression model using
[gemtc](https://CRAN.R-project.org/package=gemtc).

## Usage

``` r
covariate_model(
  configured_data,
  covariate_value,
  regressor_type,
  covariate_model_output = NULL,
  n_adapt = 5000,
  n_iter = 20000,
  async = FALSE
)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- covariate_value:

  numeric. The value at which to fit the model. Must be greater than or
  equal to the minimum value and less than or equal to the maximum value
  in `configured_data`

- regressor_type:

  character. Type of regression coefficient, either `shared`,
  `unrelated`, or `exchangeable`

- covariate_model_output:

  list. The output of the function. Default `NULL`. When supplied, only
  the output is recalculated for a given covariate value, rather than
  refitting the model.

- n_adapt:

  numeric. Number of adaptation iterations. Defaults to `5000` and can
  normally be left unchanged

- n_iter:

  numeric. Number of simulation iterations. Defaults to `20000` and can
  normally be left unchanged

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

## Value

List of [gemtc](https://CRAN.R-project.org/package=gemtc) related
output:

- mtcResults:

  model object from
  [`gemtc::mtc.run()`](https://rdrr.io/pkg/gemtc/man/mtc.run.html)
  carried through (needed to match existing code)

- mtcRelEffects:

  data relating to presenting relative effects

- rel_eff_tbl:

  table of relative effects for each comparison

- covariate_value:

  The covariate value originally passed into this function

- reference_treatment:

  character. The `reference_treatment`from `configured_data`

- comparator_names:

  Vector containing the names of the comparators

- a:

  text output stating whether fixed or random effects

- sumresults:

  summary output of relative effects

- dic:

  data frame of model fit statistics

- cov_value_sentence:

  text output stating the value for which the covariate has been set to
  for producing output

- slopes:

  named list of slopes for the regression equations (unstandardised -
  equal to one 'increment')

- intercepts:

  named list of intercepts for the regression equations at
  covariate_value

- outcome:

  character. The `outcome` from `configured_data`

- outcome_measure:

  character. The `outcome_measure`from `configured_data`

- effects:

  character. The `effects` from `configured_data`

- mtcNetwork:

  The network object from GEMTC

- covariate_min:

  Vector of minimum covariate values directly contributing to the
  regression

- covariate_max:

  Vector of maximum covariate values directly contributing to the
  regression

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

# initial model
fitted_covariate_model <- covariate_model(configured_data = configured_data,
                                          covariate_value = 98,
                                          regressor_type = "shared",
                                          n_adapt = 100,
                                          n_iter = 100)

# updated for new covariate value
updated_covariate_model <- covariate_model(configured_data = configured_data,
                                          covariate_value = 97,
                                          regressor_type = "shared",
                                          covariate_model_output = fitted_covariate_model,
                                          n_adapt = 100,
                                          n_iter = 100)
```
