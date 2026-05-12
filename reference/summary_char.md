# Characterise the data

Produce summaries of network characteristics, treatments and treatment
pairs such as the number of participants and and the mean outcome.
Inspired by `BUGSnet::net.tab()`

## Usage

``` r
summary_char(configured_data, logger = NULL)
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

List containing:

- network:

  network characteristics

- treatments:

  treatment characteristics

- pairs:

  treatment pair characteristics
