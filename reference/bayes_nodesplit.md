# Fit a Bayesian nodesplitting model

Fit a Bayesian nodesplitting model with
[`gemtc::mtc.nodesplit()`](https://rdrr.io/pkg/gemtc/man/mtc.nodesplit.html).
This is not possible for all networks and the function will return an
error if the nodes cannot be split.

## Usage

``` r
bayes_nodesplit(configured_data, n_adapt = 5000, n_iter = 20000, async = FALSE)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

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

`mtc.nodesplit` object containing an `mtc.result` object for each node

## Examples

``` r
# \donttest{
nodesplit_path <- system.file("extdata", "continuous_nodesplit.csv", package = "metainsight")
loaded_data <- setup_load(data_path = nodesplit_path,
                          outcome = "continuous")

configured_data <- setup_configure(loaded_data = loaded_data,
                                   reference_treatment = "Placebo",
                                   effects = "random",
                                   outcome_measure = "MD",
                                   ranking_option = "good",
                                   seed = 123)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

nodesplit_model <- bayes_nodesplit(configured_data,
                                   n_adapt = 100,
                                   n_iter = 100)
# }
```
