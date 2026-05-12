# Inconsistency tables for frequentist models

Produce inconsistency tables using
[`netmeta::netsplit()`](https://rdrr.io/pkg/netmeta/man/netsplit.html)

## Usage

``` r
freq_inconsistency(configured_data, logger = NULL)
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

Dataframe of inconsistency data:

- Comparison:

  Treatment comparison

- No.Studies:

  Number of studies

- NMA:

  NMA treatment effect estimate

- Direct:

  Direct treatment effect estimate

- Indirect:

  Indirect treatment effect estimate

- Difference:

  Difference between treatment effects

- Diff_95CI_lower:

  2.5% limit of difference in treatment effects

- Diff_95CI_upper:

  97.5% limit of difference in treatment effects

- pValue:

  p-value for test of "difference in treatment effects == 0"

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

freq_inconsistency(configured_data = configured_data)
#>                Comparison No.Studies    NMA Direct Indirect Difference
#> 1   the_Butcher:the_Great          2 12.200 12.200       NA         NA
#> 2  the_Butcher:the_Little          0  0.000     NA    0.000         NA
#> 3 the_Butcher:the_Younger          0  0.544     NA    0.544         NA
#> 4    the_Little:the_Great          1 12.200 12.200       NA         NA
#> 5   the_Younger:the_Great          2 11.656 11.656       NA         NA
#> 6  the_Little:the_Younger          0  0.544     NA    0.544         NA
#>   Diff_95CI_lower Diff_95CI_upper pValue
#> 1              NA              NA     NA
#> 2              NA              NA     NA
#> 3              NA              NA     NA
#> 4              NA              NA     NA
#> 5              NA              NA     NA
#> 6              NA              NA     NA
```
