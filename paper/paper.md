---
title: "MetaInsight v7: Making a comprehensive application for network meta-analysis reproducible" 
authors:
  - name: Simon Smart
    affiliation: 1
  - name: Janion Nevill
    affiliation: 1    
  - name: Tom Morris
    affiliation: 1  
  - name: Ryan Field
    affiliation: 2
  - name: Nicola Cooper
    affiliation: 1
  - name: Alex Sutton
    affiliation: 1
affiliations:
  - number: 1
    name: University of Leicester, United Kingdom
  - number: 2
    name: University of Glasgow, United Kingdom
date: 14 June 2026
bibliography: paper.bib
---

# Summary

Network meta-analysis (NMA) is a statistical method used to determine the optimal treatment for medical conditions when multiple treatment options exist.
It enables comparisons between treatments even when no individual clinical trial has compared them [@florez_2024]. 

MetaInsight ([code](https://github.com/CRSU-Apps/MetaInsight), [deployment](https://crsu.shinyapps.io/MetaInsight), [documentation](http://apps.crsu.org.uk/MetaInsight/docs), [CRAN](https://cran.r-project.org/package=metainsight)) is an R Shiny application that was developed, launching in 2018, to make NMA more accessible to clinical researchers, by providing access to cutting-edge statistical methods through a point-and-click interface available online [@owen_2019].
Development has continued ever since with the addition of new methods and visualisations [@nevill_2023; @morris_2025]. 

We present a major update, focused on refactoring to use [{shinyscholar}](https://cran.r-project.org/package=shinyscholar), a template that enables analyses to be reproduced outside of the application [@smart_2026]. 
Additionally, we make the application available as an R package, add options to upload risk of bias data, improve the quality of plots, provide a downloadable report and integrate with CINeMA.

# Statement of need 

NMA is the best-practice method to use to determine the optimal treatment when multiple treatments exist for a condition and forms part of a systematic review of the literature. 
Most NMAs are conducted by clinical researchers, who are our main target audience. 
While experts in the treatment of a particular condition, in their whole career it would be unusual to conduct more than ten NMAs and thus learning to use R, and the finer details of the required R packages, provides a substantial barrier to conducting NMAs. 
In practice, this may discourage them from ever embarking on conducting an NMA and consequently patients may not receive the optimum treatment. 

Despite the rise of LLM-enabled code production, we consider that for such critically important analyses, that can affect millions of patients, it is preferable that potentially inexperienced researchers conduct an analysis through a validated application rather than rely on LLMs. 
Open science principles demand that analyses are reproducible and this has been a substantial drawback of Shiny applications (and web applications in general). 
Specific to our use case, in the United Kingdom, the National Institute of Clinical Excellence (who evaluate evidence to determine the optimum treatments) require that NMAs are reproducible and this has limited uptake of the application.

# State of the field

MetaInsight uses established analytical routines from various R packages to conduct the analysis ([{bnma}](https://cran.r-project.org/package=bnma), [{gemtc}](https://cran.r-project.org/package=gemtc), [{meta}](https://cran.r-project.org/package=meta), [{metafor}](https://cran.r-project.org/package=metafor), [{netmeta}](https://cran.r-project.org/package=netmeta)) but these are only available to users capable of conducting analyses using R directly.

[NMAStudio](https://nmastudio.uiocloud.no/) is another web application for conducting NMAs with some functional overlap to MetaInsight, although a very different architecture (Dash and Plotly, interfacing with R) and is not reproducible [@yu_2026]. 
Previous attempts to collaborate on developing a single application have unfortunately been unsuccessful. 

# Software design
 
## Shiny

The application is built using [{shiny}](https://cran.r-project.org/package=shiny) and the main advantage of this is that the code is written entirely in R, the language used by statisticians developing methods in NMA. 
This enables direct integration and for statisticians to contribute to the codebase, making it maintainable in the long term even if budgets are limited. 
Additionally, the entire application can be deployed as a single service and many options exist for deploying Shiny apps for developers with limited experience. 

### Reactivity

Reactivity is a key feature of Shiny, automatically invalidating outputs and recalculating them when input values change.
For complex applications however, reactivity can become problematic, making the reactive graph difficult for developers to comprehend and causing unnecessary calculations to occur [@fay_2022]. 
Such problems were evident in v6, where for example, excluding a study from the sensitivity analysis also caused outputs of the main analysis to rerender. 
v7 uses [{gargoyle}](https://cran.r-project.org/package=gargoyle), an event-based system to control reactivity, this approach is explicit and comprehensible, but at the cost of added verbosity. 

## Asynchronous operations

Until 2024, long-running operations in Shiny blocked the interface both for the user and any other users connected to the same instance which was a substantial limitation. 
Fortunately, [`ExtendedTask`](https://rstudio.github.io/shiny/reference/ExtendedTask.html) in Shiny, combined with [{mirai}](https://cran.r-project.org/package=mirai) enables tasks to run tasks asynchronously, thereby maintaining responsiveness.
Users are kept informed through messages in the logger when operations begin and end. 

## Reproducibility

The lack of reproducibility in most Shiny applications is a major barrier to open science. 
[{shinymeta}](https://cran.r-project.org/package=shinymeta) was developed by the developers of Shiny to address this, but it requires unusual syntax and we are not aware of any implementations for such a complex application. 
{shinyscholar}, itself forked from [{wallace}](https://cran.r-project.org/package=wallace), was developed to build upon a proven example of a complex reproducible Shiny application. 
The justification and advantages of this approach have been described elsewhere we do not repeat them here. 

## Modifications to shinyscholar

Various modifications were made to the template however, due to the existing application containing multiple parallel analyses (up to six models), whereas shinyscholar was originally designed around a single model. 
The main modification was to introduce sub-modules to enable reuse of code to generate outputs for the main and sensitivity analysis. 
v6 already reused functions to generate outputs from different models and this was maintained, resulting in some functions being aliases of others, but maintaining a consistent API for users. 
We also added the option to render the Quarto document that reproduces the analysis to generate an html report with tabs that match the layout of the application. 
This was a desired feature in previous versions but was complicated by the need to maintain duplicate code for the app and a report. 

## Data structures

In v6, functions had many parameters including some that were immutable properties of the data, such as whether the outcome was binary or continuous.
In v7, the API has been simplified by generating a large object in `setup_configure()` which is passed to other functions as a single parameter. 
Similarly, `*_model` functions generate an object which is passed to downstream functions.

## Plots

Forest plots are a key output of NMAs and MetaInsight reuses functions from {meta} to generate them which are implemented in base R. 
These plots have very wide margins where labels for the data are written and this [presents unusual challenges to integrating with Shiny](https://simon-smart88.github.io/forest_plot_blog/forest_blog.html) as the labels take up a fixed number of pixels on the screen, resulting in unscalable plots. 
In v6, on large screens, this made the plots small, and on small screens, the plots could overlap and hide information. 
The height of plots also needs to adjust according to the number of treatments which required complicated workarounds in v6. 
To address this, all plots in v7 are all generated as scalable vector graphics via `svglite::XMLsvg()` with the height and width set inside the plot-generating function.
Plots are included in the app inside `svg_container()` so that they scale with screen size, and unlike in typical Shiny apps, rendering occurs on the client side. 
Plots can be saved to file using `write_plot()` to either png, pdf or svg without the user having to specify any dimensions.

## CINeMA integration

[CINeMA](https://cinema.med.auth.gr/) is a method and web application that researchers should use after conducting an NMA to evaluate the confidence in their findings [@nikolakopoulou_2020; @papakonstantinou_2020]. 
This is purposefully separate so that it occurs after the NMA. 
We collaborated with the developers of CINeMA to enable integration and decided that exporting results as a file that could be uploaded to CINeMA was the simplest approach. 
A json schema was developed and functions were developed to convert data to match the schema.

## Continuous integration

v6 contained some unit tests, mainly focused on validating data uploads and model outputs. 
No end-to-end tests were present and tests were only run locally. 
v7 adds unit tests for each exported function and end-to-end tests of each module using [{shinytest2}](https://cran.r-project.org/package=shinytest2) and they are run remotely using GitHub Actions. 

# Research impact statement

## Existing usage

@owen_2019 describing v1 has been cited over 330 times with citations increasing year-on-year (Scopus, June 2026). 
The majority of citations are from use of the application to conduct NMAs [@bradbury_2025]. 
Additionally, publications describing updates have been cited 75 and 3 times respectively (Scopus, June 2026) [@nevill_2023; @morris_2025]. 
This makes it one of the most highly-cited Shiny applications for conducting analyses. 
Google Analytics data for 2025 shows that users from 90 countries used v6 of the application, with 42 countries each having over ten users. 
Anecdotal evidence indicates that v6 is used in the pharmaceutical industry to evaluate new treatments against existing ones, but this usage may be hidden from analytics (if they host the application in their own secure environment) and does not produce citations.

## Integrations

v6 was integrated into [TERA Tools](https://tera-tools.com/), a collection of tools for conducting meta-analyses. 
In v7 we add integration to CINeMA, a tool for evaluating the confidence in results from NMAs. 
We are in contact with developers of tools used for data extraction to encourage them to provide exports in a compatible format.

# Remaining challenges

## Funding

Attracting funding for maintenance and development of mature software remains challenging and we were fortunate to receive support from the Chan Zuckerberg Initiative to develop most of v7. 

## User feedback

We emailed every corresponding author of publications that cited MetaInsight in 2025 to invite feedback on a beta-version of v7 but unfortunately only received one response. 
There are likely to be features which would be helpful to users, but is unclear how we can elicit them to provide feedback.

## Hosting

The application is hosted on shinyapps.io and on a server running Shiny Server provided by the University of Leicester. 
The long-term sustainability of these is uncertain however and were the application to become more popular, alternative hosting architectures may be required.

# Future directions

Most published NMAs that cite MetaInsight consider multiple outcomes as part of a systematic review, requiring them to manage and upload multiple datasets. 
Ideally, a single dataset could be uploaded and the user could select the outcome of interest.

Since MetaInsight was launched, [{multinma}](https://cran.r-project.org/package=multinma) has emerged as an alternative package for conducting Bayesian NMA, while {gemtc} is not under active development. 
Future versions could switch to use {multinma} and provide extra functionality for users.

Considering that users may use generic AI tools to interpret results with uncertain consequences, it would be preferable to integrate a chatbot with constrained access to reliable sources of information on NMA into the application.

# AI usage disclosure

Prior to v7, no AI tools were used, nor were they used to prepare this manuscript. 
During development of v7 a range of AI tools were used to assist development, but only for specific problems and all code was critically evaluated and typically refactored manually. 
Tools integrated into the IDE were not used and all architectural decisions were made by the authors. 
Specifically, DeepSeek was used for simple problems, Claude for more complex problems and DeepWiki for specific problems relating to R packages.

# Acknowledgements

The National Institute of Healthcare Research funded earlier versions and the initial development to refactor v6 to use shinyscholar as a proof-of-concept. 
Wellcome (via the Chan Zuckerburg Initiative) funded integration with CINeMA and to complete refactoring.
Impact funding from the University of Leicester funded work required to submit the package to CRAN.

# References
