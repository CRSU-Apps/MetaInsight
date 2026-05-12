# Takes the configured data, removes any excluded studies and returns subsets of the data to be passed to other functions.

Takes the configured data, removes any excluded studies and returns
subsets of the data to be passed to other functions.

## Usage

``` r
setup_exclude(configured_data, exclusions, async = FALSE)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or `setup_exclude()`

- exclusions:

  character. Vector of study names to exclude.

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`

## Value

`configured_data` containing:

- treatments:

  dataframe. Treatment names and IDs

- reference_treatment:

  character. The selected reference treatment

- connected_data:

  dataframe. A subset of the data containing only connected studies

- covariate:

  A list containing these items if covariate data exists or else empty:

&nbsp;

- `cross`: Crosses

- `circle_open`: Open circles

- `none`: No symbols in which case only the plot of direct evidence is

&nbsp;

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
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

subsetted_data <- setup_exclude(configured_data = configured_data,
                                exclusions = c("Leo", "Minerva"))
```
