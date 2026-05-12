# Creates Gelman plots for a gemtc or bnma model.

Creates Gelman plots for a gemtc or bnma model.

## Usage

``` r
gelman_plots(gelman_data, parameters)
```

## Arguments

- gelman_data:

  List of outputs from `coda:::gelman.preplot`

- parameters:

  Vector of parameters mentioned in the previous argument.

## Value

List of ggplot Gelman plots
