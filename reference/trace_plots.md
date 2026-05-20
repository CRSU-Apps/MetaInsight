# Creates trace plots of MCMC samples.

Creates trace plots of MCMC samples.

## Usage

``` r
trace_plots(model, parameters)
```

## Arguments

- model:

  Model output.

- parameters:

  Vector of parameters to create trace plots for.

## Value

List of ggplot trace plots.

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

mcmc <- bayes_mcmc(model = fitted_bayes_model)

trace_plots(fitted_bayes_model$mtcResults, mcmc$parameters)
#> [[1]]

#> 
#> [[2]]

#> 
#> [[3]]

#> 
#> [[4]]

#> 
```
