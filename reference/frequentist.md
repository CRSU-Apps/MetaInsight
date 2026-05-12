# Fit a frequentist model

Fits a frequentist model with
[`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html)

## Usage

``` r
frequentist(
  non_covariate_data,
  outcome,
  treatments,
  outcome_measure,
  effects,
  reference_treatment
)
```

## Arguments

- non_covariate_data:

  Input dataset with any covariates removed.

- outcome:

  character. Outcome type for the dataset. Either `binary` or
  `continuous`.

- treatments:

  dataframe. Treatments

- outcome_measure:

  character. Outcome measure of the dataset. Either `OR`, `RR` or `RD`
  when `outcome` is `binary` or `MD` or `SMD` when `outcome` is
  `continuous`

- effects:

  character. Type of model to fit, either `random` or `fixed`

- reference_treatment:

  character. The reference treatment of the dataset

## Value

List containing:

- netmeta:

  list. NMA results from netmeta::netmeta()

- pairwise:

  dataframe. Results from meta::pairwise() but with treatment labels

- pairwise_reversed:

  dataframe. pairwise as if the treatments had been the other way round
