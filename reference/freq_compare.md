# Compare treatments for frequentist models

Produce a comparison table of treatments using
[`netmeta::netleague()`](https://rdrr.io/pkg/netmeta/man/netleague.html).

## Usage

``` r
freq_compare(configured_data, logger = NULL)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

Dataframe of comparisons with one row and one column per treatment

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

freq_compare(configured_data = configured_data)
#>                           the_Great             the_Younger
#> the_Great                 the_Great -11.66 [-12.39; -10.92]
#> the_Younger -11.66 [-12.39; -10.92]             the_Younger
#> the_Little  -12.20 [-13.22; -11.18]  -0.54 [ -1.80;   0.71]
#> the_Butcher -12.20 [-12.93; -11.47]  -0.54 [ -1.58;   0.49]
#>                          the_Little             the_Butcher
#> the_Great   -12.20 [-13.22; -11.18] -12.20 [-12.93; -11.47]
#> the_Younger                       .                       .
#> the_Little               the_Little                       .
#> the_Butcher  -0.00 [ -1.25;   1.25]             the_Butcher
```
