# Compare treatment pairs

Produce a table of comparisons of all treatment pairs for baseline risk
models using
[`bnma::relative.effects.table()`](https://rdrr.io/pkg/bnma/man/relative.effects.table.html)

## Usage

``` r
baseline_compare(model, logger = NULL)
```

## Arguments

- model:

  list. Object produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

Relative effects table

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

baseline_compare(model = fitted_baseline_model)
#>             the_Great               the_Younger             
#> the_Great   "the_Great"             "10.9 (-80.9, 22.52)"   
#> the_Younger "-10.9 (-22.52, 80.9)"  "the_Younger"           
#> the_Butcher "-12.14 (-48.42, 2.55)" "-1.22 (-121.42, 12.14)"
#> the_Little  "-12.46 (-93.28, 4.54)" "-1.7 (-154.57, 25.05)" 
#>             the_Butcher             the_Little            
#> the_Great   "12.14 (-2.55, 48.42)"  "12.46 (-4.54, 93.28)"
#> the_Younger "1.22 (-12.14, 121.42)" "1.7 (-25.05, 154.57)"
#> the_Butcher "the_Butcher"           "0.26 (-34.9, 82.86)" 
#> the_Little  "-0.26 (-82.86, 34.9)"  "the_Little"          
```
