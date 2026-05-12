# Summarise a Bayesian model

Produce a summary of a Bayesian model

## Usage

``` r
bayes_details(model, logger = NULL)

covariate_details(...)

baseline_details(...)
```

## Arguments

- model:

  Output produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md),
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md).

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- ...:

  Parameters passed to `bayes_details()`

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

bayes_details(model = fitted_bayes_model)
#> $mcmc
#>       characteristic value
#> 1             Chains     4
#> 2 Burn-in iterations   100
#> 3  Sample iterations   100
#> 4    Thinning factor     1
#> 
#> $priors
#>                          parameter             value
#> 1       Relative treatment effects    ~ N (0, 33489)
#> 2                       Intercepts    ~ N (0, 33489)
#> 3 Heterogeneity standard deviation  ~ Unif (0, 12.2)
#> 
```
