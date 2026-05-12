# Calculate edge.weights for network

Calculate edge.weights for network

## Usage

``` r
network_structure(freq, order = NA)
```

## Arguments

- freq:

  list. Output from
  [`frequentist()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/frequentist.md)

- order:

  character. Vector of treatments names in rank order.

## Value

data.frame containing the number of studies that compare each treatment
against the reference treatment.
