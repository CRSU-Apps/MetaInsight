[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7852691.svg)](https://doi.org/10.5281/zenodo.7852691)

# MetaInsight v7.0.0
Network meta-analysis (NMA) has been increasingly adopted in evidence-based medicine to compare multiple interventions
and address the questions such as ‘which intervention is the ‘best’ overall?’. Currently, NMA is primarily conducted 
in statistical packages such as WinBUGs, R and STATA, and the software coding can be difficult for non- statisticians 
and hinders the progress of carrying out systematic reviews containing NMA.

MetaInsight is a tool that conducts NMA via the web requiring no specialist software for the user to install but 
leveraging established analysis routines (specifically the netmeta and gemtc package in R). The tool is interactive 
and uses an intuitive ‘point and click’ interface and presents results in visually intuitive and appealing ways. 
It is hoped that this tool will assist those in conducting NMA who do not have expert statistical programming skills, 
and, in turn, increase the relevance of published meta-analyses, and in the long term contribute to improved healthcare 
decision making as a result.

If you use the app please cite it as:

Owen, RK, Bradbury, N, Xin, Y, Cooper, N, Sutton, A. MetaInsight: An interactive web-based tool for analyzing, 
interrogating, and visualizing network meta-analyses using R-shiny and netmeta. Res Syn Meth. 2019; 10: 569-581.

Please consult our [Wiki](https://github.com/CRSU-Apps/MetaInsight/wiki) for further information on using MetaInsight.


## Installation and use

### JAGS

The Bayesian models used in MetaInsight rely on Just Another Gibbs Sampler (JAGS) to fit them. JAGS is a system dependency
rather than an R package and you should install JAGS first prior to installing MetaInsight. You can download JAGS from
<https://mcmc-jags.sourceforge.io/> 

### Installation

You can install MetaInsight in R using:

```r
install.packages("metainsight")
```

Or install from GitHub using:

```r
remotes::install_github("CRSU-Apps/MetaInsight")
```

Once that is complete, you can run the application with:

```r
metainsight::run_metainsight()
```

The package is mainly intended to be used through the app, but you can also use the underlying functions in R scripts.
Once the analysis has been configured using `setup_configure()` the result can be passed to all the `summary_*()` 
and `freq_*()` functions and the `baseline_model()`, `bayes_model()` and `covariate_model()` functions. The results of the
`*_model()` functions can be passed to all the other functions with the same prefix, apart from `baseline_summary()` 
and `covariate_summary()` which require `configured_data`. For example, to fit a Bayesian model and produce a forest plot you would run:

```r
library(metainsight)

loaded_data <- setup_load(data_path = "<path to file>", outcome = "continuous")
configured_data <- setup_configure(loaded_data = loaded_data, 
                                   reference_treatment = "Placebo", 
                                   effects = "random", 
                                   outcome_measure = "MD",
                                   ranking_option = "good",
                                   seed = 123)
                
fitted_model <- bayes_model(configured_data)
bayes_forest(fitted_model)
```

## For developers

### Running locally

To run the app locally, clone the repository with:

```
git clone https://github.com/CRSU-Apps/MetaInsight
```

Create an RStudio project and associate it with the directory.

Install the local version of the package with either `devtools::install_local()` or with Ctrl+Shift+B

Then run the application with `shiny::runApp("inst/shiny")`

### Application structure

v7 was built using {shinyscholar} and the app is structured as an R package and divided into components 
(Setup, Summary, Frequentist, Bayesian, Baseline risk, Covariate and Export) each containing multiple modules. 
Each module has an identifier made up of the component identifier and the module name, for example `freq_forest` 
refers to the module that produces a forest plot in the Frequentist component. Each module is made up four files 
present in `inst/shiny/modules` named using the module identifier: `<identifier>.R` contains the shiny module, 
`<identifier>.yml` contains configuration information, `<identifier>.md` contains guidance on how to use the module, 
`<identifier>.Rmd` contains an rmarkdown chunk that replicates the steps in the module. 

Bayesian modules contain sub-modules that produce the outputs for the main and sensitivity analyses respectively. 
These submodules are defined in the same `<identifier>.R` file. Some of these submodules are reused in the Baseline 
risk and Covariate components e.g. in `baseline_ranking`. Due to overlap between other modules in the Baseline risk 
and Covariate components, these modules call a `metaregression_` module which is defined in the relevant `covariate_<module>.R` 
file and reused in the `baseline_<module>.R` e.g. in `covariate_regression` and `baseline_regression`.

Each module calls a synonymous function which is found in `R/` and in a file named `<identifier>_f.R`. 
In the Baseline risk and Covariate components however, the function is an alias for that used in the equivalent Bayesian 
module and these are defined in the relevant Bayesian module's function file.

Functions which are used exclusively in the app, for example `reset_data` and `run_all` are located in `inst/shiny/ui_helpers.R`.

### Triggering events

Reactivity within and between modules is triggered using {gargoyle} which provides explicit control for when code is rerun and. 
uses the module identifier in three functions. `init(<identifier>)` is called when the app is started. 
`trigger(<identifer>)` is used at the end of `observeEvent(input$run, ...` block in each
module. `watch(<identifer>)` can be included in any code chunks that need to be rerun. Under the hood, these set a `reactiveVal` 
and each trigger adds one to the value (similar to how an `actionButton` works). This means that they can also be used to 'guard' 
code chunks by using `req(watch(<identifier>) > 0)` to only start executing code once the trigger can has been used. In some modules
it is necessary to add another trigger, for example in the `bayes_model` module, the trigger fits both models, but 
events that are triggered by the model being fitted need to listen to `bayes_model_all` or `bayes_model_sub`. When this is the
case, you need to add `init()` at the start of the module.

### Hiding and showing content

The module identifier is also used as a CSS class to hide and show content like download buttons or explanatory text in the 
Results panel. Wrap content inside `div(class = <identifier>, ...)` to enable this. By default `hide_and_show(<identifier>)` will hide 
content until `trigger(<identifier>)` is called, but you can override this (for example when a function is slow to run and you 
want to wait until it is complete) by using `hide_and_show(<identifier>, show = FALSE)` and then including 
`on.exit(shinyjs::show(selector = paste0(".", <identifier>)))` inside a function. If the app is reset, the content will be 
hidden again.

### Plots

Functions that produce plots available to download (i.e. excluding the MCMC and deviance plots) generate them as a 
scalable vector graphic (SVG) which ensures consistency and scalability. These are generated in the function by placing 
code to generate the plot inside `svglite::xmlSVG()` and piping this to `crop_svg()`. As in previous versions, the plot 
height and width may need to be calculated according to the data (how many treatments etc.) and these should be supplied 
as `width` and `height` parameters to `svglite::xmlSVG()`:

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

They are displayed in the app by calling the function inside a `reactive()` and wrapping the output inside `svg_container()` 
(which also adds a button to enable fullscreen viewing) placed in a `renderUI()`. The output can be passed to `write_plot()` 
to download the plot as either a `.png`, `.pdf` or `.svg`.  This also enables saving outside of the application for example 
using e.g. `plotting_function() |> write_plot("plot.png")`

If you call the plotting function in an `.Rmd` of `.qmd` chunk, the plot will be displayed, but if calling it from an R terminal, pipe it to
`htmltools::browsable()` to show the plot in the Viewer pane of RStudio.

```r

### Inside server function

svg <- reactive({
  plotting_function()
}) 

output$plot <- renderUI({
  req(svg())
  svg_container(
      svg()
  )
})

output$download <- downloadHandler(
  filename = function() {
    glue("MetaInsight_plot_name.{common$download_format}")
  },
  content = function(file) {
    write_plot(svg(), file)
  }
)

### Inside results function

uiOutput(ns("plot"))

```

### Updating modules or creating new modules

Development of new modules or modification of existing modules should generally begin by creating or editing the module function. 
Running `devtools::load_all(".")` or using Ctrl+Shift+L will load all the package functions and data that you need. This will take a
few minutes the first time, but afterwards will take a few seconds.
Function development should occur outside of the application and after running `load_all()` you will have access to all the data 
required for development, for example `configured_data_con` is the configured data for a continuous outcome and `configured_data_bin`
is the configured data for a binary outcome, `fitted_bayes_model` is a Bayesian model, `fitted_baseline_model` is a baseline risk
model and `fitted_covariate_model` is a covariate model. To be able to use the function once you have developed it or modified it, 
you need to document and reinstall the package with Ctrl+Shift+D and Ctrl+Shift+B.

You can create a new module from scratch using `shinyscholar::create_module()` but it is probably easier to adapt an existing module 
that is similar to the one you are developing. Once the function is developed, you can also use save files to restore the app 
state by setting a variable called `load_file_path` to the path of the saved file. For example to test a module using the result
of the Bayesian models, run `load_file_path <- bayes_model_path` and run the app. This enables making rapid changes to the module 
under development without having to rerun the preceding modules. Unlike when developing the function, modules are loaded whenever 
the app is run, so you do not need to reinstall the package in order to see your changes.

Each module should have unit tests that check that the function works correctly written with `{testthat}` and end-to-end tests that 
check that the function works correctly inside the app written with `{shinytest2}`. These are located in `tests/testthat` in a file 
called `test-<identifier>.R`. It is probably easiest to use tests for an existing module as the basis for the new module.

### Testing

After making changes, you should run the tests for the module you have worked on locally and then run the full test suite on GitHub. To run 
the tests, open the relevant file in `tests/testthat/`, run `devtools::load_all(".")` and then select all the tests you 
want to run and hit Ctrl+Enter, or just click the Run Tests button in the top right of the window pane.

On GitHub, the end-to-end tests do not run reliably on Windows so are skipped which conveniently matches the tests that CRAN run. 
Similarly, the `export_markdown` tests are skipped on Mac due to a problem linking JAGS and Quarto.




