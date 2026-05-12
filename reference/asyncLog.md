# asyncLog

For internal use. Similar to writeLog but for use inside async functions

## Usage

``` r
asyncLog(async, ..., type = "default")
```

## Arguments

- async:

  Whether the function is being used asynchronously

- ...:

  Messages to write to the logger

- type:

  One of `default`, `info`, `error`, `warning`

## Value

No return value, called for side effects
