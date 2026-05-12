# Convert output of `bnma::relative.effects.table()` into a suitable format to pass to `gemtc::blobbogram()`

Convert output of
[`bnma::relative.effects.table()`](https://rdrr.io/pkg/bnma/man/relative.effects.table.html)
into a suitable format to pass to
[`gemtc::blobbogram()`](https://rdrr.io/pkg/gemtc/man/blobbogram.html)

## Usage

``` r
baseline_forest_format(median_ci_table, reference_treatment)
```

## Arguments

- median_ci_table:

  matrix. Created by
  [`bnma::relative.effects.table()`](https://rdrr.io/pkg/bnma/man/relative.effects.table.html)

- reference_treatment:

  character. The reference treatment of the dataset
