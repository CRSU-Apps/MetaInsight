# Baseline regression data

Generate data required to produce a metaregression plot for a baseline
risk model.

## Usage

``` r
baseline_regression(model, configured_data, async = FALSE)
```

## Arguments

- model:

  Output produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)

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

# n_iter, max_iter and check_iter are set low to run quickly, but should
# be left as the default values in real use

fitted_baseline_model <- baseline_model(configured_data = configured_data,
                                        regressor_type = "shared",
                                        n_iter = 120,
                                        max_iter = 120,
                                        check_iter = 10)

regression_data <- baseline_regression(model = fitted_baseline_model,
                                       configured_data = configured_data)
```
