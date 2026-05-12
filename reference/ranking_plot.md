# Treatment ranking plots

Produce either a rankogram or radial SUCRA plot ranking the treatments

## Usage

``` r
ranking_plot(
  ranking_data,
  style,
  colourblind = FALSE,
  simple = FALSE,
  regression_text = "",
  logger = NULL
)
```

## Arguments

- ranking_data:

  list created by
  [`baseline_ranking()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_ranking.md),
  [`bayes_ranking()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_ranking.md)
  or
  [`covariate_ranking()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_ranking.md).

- style:

  character. The style of plot to produce. Either `rankogram` or
  `radial`

- colourblind:

  logical. Whether to use a colourblind-friendly palette. Defaults to
  `FALSE`.

- simple:

  logical. Whether to display a simplified version of the radial plot.
  Does not affect the rankogram plot. Defaults to `FALSE`

- regression_text:

  Text to show for regression. Defaults to no text.

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

html. Contains the svg string to generate the plot. This will display
the plot when at the end of a quarto or rmarkdown chunk. To view in the
viewer panel of Rstudio, use
[`htmltools::browsable()`](https://rstudio.github.io/htmltools/reference/browsable.html).
The output can be saved using
[`write_plot()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/write_plot.md).
