# export_cinema

Prepare project into a JSON format that CINeMA can read.

## Usage

``` r
export_cinema(configured_data, gemtc_results = NULL, logger = NULL)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- gemtc_results:

  Output from
  [`gemtc::mtc.run()`](https://rdrr.io/pkg/gemtc/man/mtc.run.html), as
  returned in the `mtcResults` list element from
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md).
  If this parameter is `NULL` then the frequentist analysis results
  found in 'contributions' are used. If it is not `NULL` then the
  Bayesian analysis results contained in this parameter are used.
  Defaults to `NULL`.

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

JSON string with the following structure: A named list of lists. The top
level list contains items:

- "project" Information for CINeMA project

  - "CM" Contribution matrices

    - "contributionMatrices" output from `.PrepareAnalysisForCinema()`

  - "format" Data format. Always "long"

  - "type" Outcome type. Either "binary" or "continuous"

  - "Studies" Study data

    - "long" Output from `.PrepareDataForCinema()`

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

cinema_project <- export_cinema(configured_data = configured_data)

writeLines(cinema_project, tempfile(fileext = ".json"))
```
