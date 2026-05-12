# Fit a Bayesian model

Fit a Bayesian model using
[gemtc](https://CRAN.R-project.org/package=gemtc)

## Usage

``` r
bayes_model(configured_data, n_adapt = 5000, n_iter = 20000, async = FALSE)
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

List containing:

- mtcResults:

  mtc.result. Output from
  [`gemtc::mtc.run()`](https://rdrr.io/pkg/gemtc/man/mtc.run.html)

- mtcRelEffects:

  mtc.result. Output from
  [`gemtc::relative.effect()`](https://rdrr.io/pkg/gemtc/man/relative.effect.html)

- rel_eff_tbl:

  mtc.relative.effect.table. Output from
  [`gemtc::relative.effect.table()`](https://rdrr.io/pkg/gemtc/man/relative.effect.table.html)

- sumresults:

  summary.mtc.result. Output from `summary(mtcRelEffects)`

- mtcNetwork:

  mtc.network. Output from
  [`gemtc::mtc.network()`](https://rdrr.io/pkg/gemtc/man/mtc.network.html)

- dic:

  dataframe. Containing the statistics 'Dbar', 'pD', 'DIC', and 'data
  points'

- outcome:

  character. The `outcome` from `configured_data`

- outcome_measure:

  character. The `outcome_measure`from `configured_data`

- reference_treatment:

  character. The `reference_treatment`from `configured_data`

- effects:

  character. The `effects` from `configured_data`

- seed:

  numeric. The `seed` from `configured_data`

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)
```
