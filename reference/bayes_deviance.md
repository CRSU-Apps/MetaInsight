# Produce deviance plots

Produce deviance plots using the output of
[`gemtc::mtc.deviance()`](https://rdrr.io/pkg/gemtc/man/mtc.deviance.html)
for Bayesian and covariate models. Because these plots are interactive,
it is not currently possible to download them, although they can be
included in html reports.

## Usage

``` r
bayes_deviance(model, n_adapt = 5000, n_iter = 20000, async = FALSE)

covariate_deviance(...)
```

## Arguments

- model:

  Bayesian model produced by
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- n_adapt:

  numeric. Number of adaptation iterations. Defaults to `5000` and can
  normally be left unchanged

- n_iter:

  numeric. Number of simulation iterations. Defaults to `20000` and can
  normally be left unchanged

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

- ...:

  Parameters passed to `bayes_deviance()`

## Value

A list containing different elements depending on the input model:

When `model` was created by
[`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
containing:

- deviance_mtc:

  results from
  [`gemtc::mtc.deviance()`](https://rdrr.io/pkg/gemtc/man/mtc.deviance.html)
  for model\$mtcResults

- deviance_ume:

  results from
  [`gemtc::mtc.deviance()`](https://rdrr.io/pkg/gemtc/man/mtc.deviance.html)
  for UME model

- scat_plot:

  plotly object

- stem_plot:

  plotly object

- lev_plot:

  plotly object

When `model` was created by
[`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)
containing:

- deviance_mtc:

  results from
  [`gemtc::mtc.deviance()`](https://rdrr.io/pkg/gemtc/man/mtc.deviance.html)
  for model\$mtcResults

- stem_plot:

  plotly object

- lev_plot:

  plotly object

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

bayes_deviance(model = fitted_bayes_model,
               n_adapt = 100,
               n_iter = 100)
#> $deviance_mtc
#> $Dbar
#> [1] 10.26853
#> 
#> $pD
#> [1] 9.915554
#> 
#> $DIC
#> [1] 20.18408
#> 
#> $`data points`
#> [1] 10
#> 
#> $dev.ab
#>                  [,1]      [,2]
#> Constantine 1.0289917 1.0731834
#> Justinian   0.8952413 1.0040952
#> Leo         1.0361644 0.9935584
#> Minerva     1.1031708 1.0751766
#> Nero        1.0667055 0.9922414
#> 
#> $dev.re
#> NULL
#> 
#> $fit.ab
#>                    [,1]        [,2]
#> Constantine 0.012273095 0.031399703
#> Justinian   0.007264789 0.005550462
#> Leo         0.174964730 0.083820254
#> Minerva     0.023230402 0.006405278
#> Nero        0.001458993 0.006606685
#> 
#> $fit.re
#> NULL
#> 
#> $lev.ab
#>                  [,1]      [,2]
#> Constantine 1.0167186 1.0417837
#> Justinian   0.8879765 0.9985447
#> Leo         0.8611997 0.9097382
#> Minerva     1.0799404 1.0687713
#> Nero        1.0652465 0.9856347
#> 
#> $lev.re
#> NULL
#> 
#> $nd.ab
#> Constantine   Justinian         Leo     Minerva        Nero 
#>           2           2           2           2           2 
#> 
#> $nd.re
#> NULL
#> 
#> $fitted
#> $fitted$theta
#>           [,1]      [,2]
#> [1,]  1.079774 12.231826
#> [2,] 15.485988  3.312417
#> [3,]  2.272815 14.350348
#> [4,]  5.524099 17.687501
#> [5,] 16.593884  4.413186
#> 
#> 
#> attr(,"class")
#> [1] "mtc.deviance"
#> 
#> $deviance_ume
#> $Dbar
#> [1] 10.24259
#> 
#> $pD
#> [1] 9.912225
#> 
#> $DIC
#> [1] 20.15481
#> 
#> $`data points`
#> [1] 10
#> 
#> $dev.ab
#>                  [,1]      [,2]
#> Constantine 0.8892939 0.9756528
#> Justinian   1.0003473 0.9540050
#> Leo         1.0378612 1.1126090
#> Minerva     1.2057239 1.0778474
#> Nero        1.0044067 0.9848403
#> 
#> $dev.re
#> NULL
#> 
#> $fit.ab
#>                    [,1]        [,2]
#> Constantine 0.017871613 0.042603096
#> Justinian   0.012047075 0.002301228
#> Leo         0.120319223 0.088122815
#> Minerva     0.041308614 0.003978194
#> Nero        0.000616155 0.001194353
#> 
#> $fit.re
#> NULL
#> 
#> $lev.ab
#>                  [,1]      [,2]
#> Constantine 0.8714223 0.9330497
#> Justinian   0.9883003 0.9517038
#> Leo         0.9175419 1.0244862
#> Minerva     1.1644153 1.0738692
#> Nero        1.0037906 0.9836460
#> 
#> $lev.re
#> NULL
#> 
#> $nd.ab
#> Constantine   Justinian         Leo     Minerva        Nero 
#>           2           2           2           2           2 
#> 
#> $nd.re
#> NULL
#> 
#> $fitted
#> $fitted$theta
#>           [,1]      [,2]
#> [1,]  1.075593 12.237071
#> [2,] 15.481956  3.307995
#> [3,]  2.260382 14.349090
#> [4,]  5.532136 17.690150
#> [5,] 16.596025  4.405606
#> 
#> 
#> attr(,"class")
#> [1] "mtc.deviance"
#> 
#> $scat_plot
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
