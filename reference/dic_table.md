# Create a summary table of deviance information criterion stats for Bayesian models

Create a summary table of deviance information criterion stats for
Bayesian models

## Usage

``` r
dic_table(dic, analysis = "all")
```

## Arguments

- dic:

  dataframe of DIC stats from `baseline_model`,
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- analysis:

  Whether the analysis is using all studies (`all`) or a subset (`sub`)
