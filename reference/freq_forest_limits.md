# Extract the minimum and maximum confidence intervals from the summary produced by netmeta

Extract the minimum and maximum confidence intervals from the summary
produced by netmeta

## Usage

``` r
freq_forest_limits(freq, outcome)
```

## Arguments

- freq:

  list. NMA results created by freq_wrap().

- outcome:

  character. `binary` or `continuous`

## Value

List containing:

- xmin:

  numeric. Minimum confidence interval

- xmax:

  numeric. Maximum confidence interval
