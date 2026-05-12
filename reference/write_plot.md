# Write plots to a file

Write an svg plot to either a png, pdf or svg file.

## Usage

``` r
write_plot(svg, file)
```

## Arguments

- svg:

  html. containing the svg string, returned from `crop_svg()`

- file:

  character. The file to which to write.

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

tmp <- tempfile(fileext = ".png")
summary_network(configured_data = configured_data,
                style = "netgraph") |>
                write_plot(tmp)
unlink(tmp)
```
