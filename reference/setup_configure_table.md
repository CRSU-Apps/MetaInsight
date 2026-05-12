# Summarise the analysis configuration

Create a table summarising how the analysis has been configured

## Usage

``` r
setup_configure_table(configured_data)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

setup_configure_table(configured_data)
#>                                                                   value
#> Outcome type:                                                Continuous
#> Outcome measure:                                        Mean Difference
#> Model effects type:                                              Random
#> Reference treatment:                                          The_great
#> For treatment rankings, values lower than the mean are:       Desirable
#> Seed value:                                                         123
```
