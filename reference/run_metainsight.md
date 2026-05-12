# Run *"metainsight"* Application

This function runs the *"metainsight"* application in the user's default
web browser.

## Usage

``` r
run_metainsight(
  launch.browser = TRUE,
  port = getOption("shiny.port"),
  load_file = NULL
)
```

## Arguments

- launch.browser:

  Whether or not to launch a new browser window.

- port:

  The port for the shiny server to listen on. Defaults to a random
  available port.

- load_file:

  Path to a saved session file which will be loaded when the app is
  opened

## Author

Jamie Kass <jkass@gradcenter.cuny.edu>

Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@gradcenter.cuny.edu>

Simon E. H. Smart <simon.smart@cantab.net>

## Examples

``` r
if(interactive()) {
run_metainsight()
}
```
