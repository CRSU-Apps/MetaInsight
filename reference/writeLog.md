# writeLog

For internal use. Add text to a logger

## Usage

``` r
writeLog(logger, ..., type = "default", go_to = NULL)
```

## Arguments

- logger:

  The logger to write the text to. Can be NULL or a function

- ...:

  Messages to write to the logger

- type:

  One of "default", "info", "error", "warning"

- go_to:

  character. The id of a module to navigate to when the modal is closed.
  Only used when `type = "error"`
