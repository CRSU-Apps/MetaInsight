# Produce an version of the `summary_study()` plot for use in the interface for excluding studies. Inside the app this is interactive, but it can also be rendered for non-interactive use.

Produce an version of the
[`summary_study()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/summary_study.md)
plot for use in the interface for excluding studies. Inside the app this
is interactive, but it can also be rendered for non-interactive use.

## Usage

``` r
setup_exclude_plot(configured_data, exclusions = NULL, hover = FALSE)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- exclusions:

  character. Vector of excluded studies. Defaults to `NULL`, but can be
  used to reset on loading

- hover:

  logical. Whether change the cursor on clickable lines. Defaults to
  `FALSE`

## Value

html. Contains the svg string to generate the plot. This will display
the plot when at the end of a quarto or rmarkdown chunk. To view in the
viewer panel of Rstudio, use
[`htmltools::browsable()`](https://rstudio.github.io/htmltools/reference/browsable.html).
The output can be saved using
[`write_plot()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/write_plot.md).
