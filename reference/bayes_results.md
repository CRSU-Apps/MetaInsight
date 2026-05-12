# Summarise a Bayesian model

Produce a table summarising Bayesian models

## Usage

``` r
bayes_results(model, logger = NULL)

covariate_results(...)

baseline_results(...)
```

## Arguments

- model:

  list. Output produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md),
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md).

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- ...:

  Parameters passed to `bayes_results()`

## Value

HTML summary of the model

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

bayes_results(fitted_bayes_model)
#> The results are on the Mean Difference scale.<br/><br/>Iterations = 101:200<br/>Thinning interval = 1<br/>Number of chains = 4<br/>Sample size per chain = 100
```
