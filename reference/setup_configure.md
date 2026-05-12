# Configure the analysis

Checks the connectivity of the loaded data and converts it into formats
for later analyses. Conducts a frequentist analysis using
[`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html).
The output can be passed to many other functions - all `summary_` and
`freq_` functions and
[`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md),
[`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)
and
[`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md).

## Usage

``` r
setup_configure(
  loaded_data,
  reference_treatment,
  effects,
  outcome_measure,
  ranking_option,
  seed,
  logger = NULL
)
```

## Arguments

- loaded_data:

  list. Output from
  [`setup_load()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_load.md)

- reference_treatment:

  character. The reference treatment of the dataset

- effects:

  character. Type of model to fit, either `random` or `fixed`

- outcome_measure:

  character. Outcome measure of the dataset. Either `OR`, `RR` or `RD`
  when `outcome` is `binary` or `MD` or `SMD` when `outcome` is
  `continuous`

- ranking_option:

  character. `good` if the treatment effect is desirable, else `bad`

- seed:

  numeric. Seed used to fit the models.

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

`configured_data` containing:

- treatments:

  dataframe. Treatment names and IDs

- reference_treatment:

  character. The selected reference treatment

- disconnected_indices:

  vector. Indices of studies that are not connected to the main network

- connected_data:

  dataframe. A subset of the data containing only connected studies

- non_covariate_data:

  dataframe. The uploaded data with covariates removed

- covariate:

  A list containing these items if covariate data exists or else empty:

  column

  :   character. Name of the column containing covariate data

  name

  :   character. Name of the covariate

  type

  :   character. Whether the covariate is `binary` or `continuous`

- freq:

  list. Processed data for frequentist analyses created by
  [`frequentist()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/frequentist.md)

- outcome:

  character. Whether the data is `binary` or `continuous`

- outcome_measure:

  character. Outcome measure of the dataset.

- effects:

  character. Whether the models are `fixed` or `random` effects

- ranking_option:

  character. Whether higher values in the data are `good` or `bad`

- seed:

  numeric. A seed value to be passed to models

## Examples

``` r
minimal_data_path <- system.file("extdata", "continuous_minimal.csv", package = "metainsight")
loaded_data <- setup_load(data_path = minimal_data_path,
                          outcome = "continuous")

configured_data <- setup_configure(loaded_data = loaded_data,
                                   reference_treatment = "the Great",
                                   effects = "random",
                                   outcome_measure = "MD",
                                   ranking_option = "good",
                                   seed = 123)
```
