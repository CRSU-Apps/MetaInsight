# Covariate regression data

Generate data from a covariate model required to produce a
metaregression plot

## Usage

``` r
covariate_regression(model, configured_data, async = FALSE)
```

## Arguments

- model:

  list. Output created by
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

## Value

List containing:

- directness:

  list. Output from `CalculateDirectness()`

- credible_regions:

  list. Output from `CalculateCredibleRegions()`

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_covariate_model <- covariate_model(configured_data = configured_data,
                                          covariate_value = 98,
                                          regressor_type = "shared",
                                          n_adapt = 100,
                                          n_iter = 100)

regression_data <- covariate_regression(model = fitted_covariate_model,
                                        configured_data = configured_data)
```
