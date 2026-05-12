# Fit a baseline risk regression model

Fit a baseline risk regression model using
[`bnma::network.run()`](https://rdrr.io/pkg/bnma/man/network.run.html).
The output is consistent with outputs produced by
[gemtc](https://CRAN.R-project.org/package=gemtc).

## Usage

``` r
baseline_model(
  configured_data,
  regressor_type,
  n_iter = 20000,
  max_iter = 60000,
  check_iter = 10000,
  async = FALSE
)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- regressor_type:

  character. Type of regression coefficient, either `shared`,
  `unrelated`, or `exchangeable`

- n_iter:

  numeric. Number of simulation iterations. Defaults to `20000` and can
  normally be left unchanged

- max_iter:

  numeric. The maximum number of iterations. Defaults to `60000` and can
  normally be left unchanged.

- check_iter:

  numeric. The number of iterations after which convergence is checked
  for. Defaults to `10000` and can normally be left unchanged.

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

## Value

List of bnma related output:

- mtcResults:

  model object itself carried through (needed to match existing code)

- covariate_value:

  The mean covariate value, used for centring

- reference_treatment:

  character. The `reference_treatment`from `configured_data`

- comparator_names:

  Vector containing the names of the comparators

- a:

  text output stating whether fixed or random effects

- cov_value_sentence:

  text output stating the value for which the covariate has been set to
  for producing output

- slopes:

  named list of slopes for the regression equations (unstandardised -
  equal to one 'increment')

- intercepts:

  named list of intercepts for the regression equations at cov_value

- outcome:

  character. The `outcome` from `configured_data`

- outcome_measure:

  character. The `outcome_measure`from `configured_data`

- effects:

  character. The `effects` from `configured_data`

- covariate_min:

  Vector of minimum covariate values directly contributing to the
  regression

- covariate_max:

  Vector of maximum covariate values directly contributing to the
  regression

- dic:

  Summary of model fit

- sumresults:

  Output of summary(model)

- regressor:

  Type of regression coefficient

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_iter, max_iter and check_iter are set low to run quickly, but should
# be left as the default values in real use

fitted_baseline_model <- baseline_model(configured_data = configured_data,
                                        regressor_type = "shared",
                                        n_iter = 120,
                                        max_iter = 120,
                                        check_iter = 10)
```
