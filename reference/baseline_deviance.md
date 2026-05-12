# Produce deviance plots for baseline risk models

Produce deviance plotly plots for baseline risk models. Unlike for
`bayes_model` output, only stem and leverage plots are produced.

## Usage

``` r
baseline_deviance(model, async = FALSE)
```

## Arguments

- model:

  Output model produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

## Value

list containing:

- deviance_mtc:

  equivalent summary to that produced by
  [`gemtc::mtc.deviance()`](https://rdrr.io/pkg/gemtc/man/mtc.deviance.html)

- stem_plot:

  plotly object

- lev_plot:

  plotly object

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

baseline_deviance(model = fitted_baseline_model)
#> $deviance_mtc
#> $dev.ab
#>           [,1]      [,2]
#> [1,] 1.0575401 0.9867496
#> [2,] 1.1049775 1.0590197
#> [3,] 1.2316922 1.0094592
#> [4,] 1.1417250 0.9548742
#> [5,] 0.7941418 0.8924938
#> 
#> $fit.ab
#>             [,1]         [,2]
#> [1,] 0.008295011 0.0005403835
#> [2,] 0.010604612 0.0127564317
#> [3,] 0.000948511 0.0002254921
#> [4,] 0.058524289 0.0060517419
#> [5,] 0.004213157 0.0010907701
#> 
#> $dev.re
#> NULL
#> 
#> $fit.re
#> NULL
#> 
#> $nd.ab
#> [1] 2 2 2 2 2
#> 
#> $nd.re
#> NULL
#> 
#> attr(,"class")
#> [1] "mtc.deviance"
#> 
#> $stem_plot
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> A marker object has been specified, but markers is not in the mode
#> Adding markers to the mode...
#> 
#> $lev_plot
#> No trace type specified:
#>   Based on info supplied, a 'scatter' trace seems appropriate.
#>   Read more about this trace type -> https://plotly.com/r/reference/#scatter
#> No trace type specified:
#>   Based on info supplied, a 'scatter' trace seems appropriate.
#>   Read more about this trace type -> https://plotly.com/r/reference/#scatter
#> No trace type specified:
#>   Based on info supplied, a 'scatter' trace seems appropriate.
#>   Read more about this trace type -> https://plotly.com/r/reference/#scatter
#> No trace type specified:
#>   Based on info supplied, a 'scatter' trace seems appropriate.
#>   Read more about this trace type -> https://plotly.com/r/reference/#scatter
#> No trace type specified:
#>   Based on info supplied, a 'scatter' trace seems appropriate.
#>   Read more about this trace type -> https://plotly.com/r/reference/#scatter
#> No scatter mode specifed:
#>   Setting the mode to markers
#>   Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode
#> 
```
