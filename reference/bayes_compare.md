# Compare treatment pairs

Produce a table of comparisons of all treatment pairs for Bayesian
models using
[`gemtc::relative.effect.table()`](https://rdrr.io/pkg/gemtc/man/relative.effect.table.html)

## Usage

``` r
bayes_compare(model, logger = NULL)

covariate_compare(...)
```

## Arguments

- model:

  list. Object created by
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- ...:

  Parameters passed to `bayes_compare()`

## Value

Relative effects table

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

bayes_compare(model = fitted_bayes_model)
#>                     the_Butcher              the_Great          the_Little
#> the_Butcher         the_Butcher -12.13 (-15.41, -8.66) -0.08 (-6.65, 5.42)
#> the_Great   12.13 (8.66, 15.41)              the_Great  12.1 (6.92, 16.29)
#> the_Little   0.08 (-5.42, 6.65)  -12.1 (-16.29, -6.92)          the_Little
#> the_Younger   0.51 (-3.7, 5.36) -11.65 (-14.68, -8.13)  0.47 (-6.26, 5.91)
#>                     the_Younger
#> the_Butcher  -0.51 (-5.36, 3.7)
#> the_Great   11.65 (8.13, 14.68)
#> the_Little  -0.47 (-5.91, 6.26)
#> the_Younger         the_Younger
```
