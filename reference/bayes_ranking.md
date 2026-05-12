# Treatment rankings

Generate treatment ranking data required to produce SUCRA plots from
Bayesian models

## Usage

``` r
bayes_ranking(model, configured_data, logger = NULL)

baseline_ranking(...)

covariate_ranking(...)
```

## Arguments

- model:

  list. Output produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md),
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md).

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- ...:

  Parameters passed to `bayes_ranking()`

## Value

List of output created by `rankdata()`

- SUCRA:

  Dataframe of SUCRA data

- Colour:

  Dataframe of colours

- Cumulative:

  Dataframe of cumulative ranking probabilities

- Probabilities:

  Dataframe of ranking probabilities

- Network:

  Dataframe of network characteristics

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

ranking_data <- bayes_ranking(fitted_bayes_model, configured_data)
```
