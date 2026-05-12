# Upgrade old data formats

Loads a `.csv` file and converts it into a suitable format for use by
`setup_load`.

## Usage

``` r
setup_upgrade(data_path, treatments, logger = NULL)
```

## Arguments

- data_path:

  character. Path to the file to be upgraded

- treatments:

  character. The treatments in the data separated by commas.

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

Dataframe containing the upgraded data

## Examples

``` r
old_data_path <- system.file("extdata", "old_data.csv", package = "metainsight")
upgraded_data <- setup_upgrade(data_path = old_data_path,
                               treatments = "A,B,C,D,E")
```
