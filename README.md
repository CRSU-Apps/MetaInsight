[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7852691.svg)](https://doi.org/10.5281/zenodo.7852691)

# MetaInsight v7.0.0
Network meta-analysis (NMA) has been increasingly adopted in evidence-based medicine to compare multiple interventions and address the questions such as ‘which intervention is the ‘best’ overall?’. Currently, NMA is primarily conducted in statistical packages such as WinBUGs, R and STATA, and the software coding can be difficult for non- statisticians and hinders the progress of carrying out systematic reviews containing NMA.

MetaInsight is a tool that conducts NMA via the web requiring no specialist software for the user to install but leveraging established analysis routines (specifically the netmeta and gemtc package in R). The tool is interactive and uses an intuitive ‘point and click’ interface and presents results in visually intuitive and appealing ways. It is hoped that this tool will assist those in conducting NMA who do not have expert statistical programming skills, and, in turn, increase the relevance of published meta-analyses, and in the long term contribute to improved healthcare decision making as a result.

If you use the app please cite it as:

Owen, RK, Bradbury, N, Xin, Y, Cooper, N, Sutton, A. MetaInsight: An interactive web-based tool for analyzing, interrogating, and visualizing network meta-analyses using R-shiny and netmeta. Res Syn Meth. 2019; 10: 569-581.

Please consult our [Wiki](https://github.com/CRSU-Apps/MetaInsight/wiki) for further information on using MetaInsight.


## Application structure

v7 was built using *shinyscholar* and the app is structured as an R package and divided into components (Setup, Summary, Frequentist, Bayesian, Baseline risk, Covariate and Reproduce) each containing multiple modules. Each module has an identifier made up of the component identifier and the module name, for example `freq_forest` refers to the module that produces a forest plot in the Frequentist component. Each module is made up four files present in `inst/shiny/modules` named using the module identifier: `<identifier>.R` contains the shiny module, `<identifier>.yml` contains configuration information, `<identifier>.md` contains guidance on how to use the module, `<identifier>.Rmd` contains an rmarkdown chunk that replicates the steps in the module. 

Bayesian modules contain sub-modules that produce the outputs for the main and sensitivity analyses respectively. These submodules are defined in the same `<identifier>.R` file. Some of these submodules are reused in the Baseline risk and Covariate components e.g. in `baseline_ranking`. Due to overlap between other modules in the Baseline risk and Covariate components, these modules call a `metaregression_` module which is defined in the relevant `covariate_<module>.R` file and reused in the `baseline_<module>.R` e.g. in `covariate_regression` and `baseline_regression`.

Each module calls a synonymous function which is found in `R/` and in a file named `<identifier>_f.R`. In the Baseline risk and Covariate components however, the function is an alias for that used in the equivalent Bayesian module and these are defined in the relevant Bayesian module's function file.


## Plots

Functions that produce plots available to download (i.e. excluding the MCMC and deviance plots) generate them as a scalable vector graphics (SVG) which ensures consistency and scalability. These are generated in the function by placing code to generate the plot inside `svglite::xmlSVG()` and piping this to `crop_svg()`. As in previous versions, the plot height and width may need to be calculated according to the data (how many treatments etc.) and these should be supplied as `width` and `height` parameters to `svglite::xmlSVG()`:

```r
plotting_function <- function(){
  svglite::xmlSVG({
    plot(1:10, 1:10)
  },
  width = 8,
  height = 8
  ) |> crop_svg()
}
```

They are displayed in the app by calling the function inside a `reactive()` and wrapping the output inside a `div()` with the class `svg_container` placed in a `renderUI()`. The output can be passed to `write_plot()` to download the plot as either an `.png`, `.pdf` or `.svg`.  This also enables saving outside of the application for example using e.g. `plotting_function() |> write_plot("plot.png", ".png")

```r

### SERVER

svg <- reactive({
  plotting_function()
}) 

output$plot <- renderUI({
  req(svg())
  div(class = "svg_container",
      svg()
  )
})

output$download <- downloadHandler(
  filename = function() {
    glue("MetaInsight_plot_name.{common$download_format}")
  },
  content = function(file) {
    write_plot(svg(),
               file,
               common$download_format
               )

  }
)

### UI

uiOutput(ns("plot"))

```

## Development and debugging

Development of new modules or modification of existing modules should generally begin by creating or editing the module function. This should occur outside of the application, but you may find it helpful to save the app state at the relevant point prior to where the change is to be made so that the same data can be used in development. For example, if developing a new module for the Frequentist component, run the `setup_load` and `setup_configure` modules, save the app and then read the data in using `common <- readRDS(<save_file>.rds)` you can then access the data using e.g. `common$freq_all`. 

You can create a new module from scratch using `shinyscholar::create_module()` but it is probably easier to adapt an existing module that is similar to the one you are developing. Once the function is developed, you can also use the saved file to restore the app state by setting a variable called `load_file_path` to the path of the saved file. This enables making rapid changes to the module under development without having to rerun the preceding modules.

Each module should have unit tests that check that the function works correctly written with `{testthat}` and end-to-end tests that check that the function works correctly inside the app written with `{shinytest2}`. These are located in `tests/testthat` in a file called `test-<identifier>.R`. It is probably easiest to use tests for an existing module as the basis for the new module.
