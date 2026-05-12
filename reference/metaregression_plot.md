# Produce a meta-regression plot

Produce a composite meta-regression plot which comprises plots showing
direct and indirect evidence from baseline or covariate models. The
design was adapted from Donegan et al. (2018)
<https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1292>

## Usage

``` r
metaregression_plot(
  model,
  configured_data,
  regression_data,
  comparators,
  include_covariate = FALSE,
  include_ghosts = FALSE,
  include_extrapolation = FALSE,
  include_credible = FALSE,
  credible_opacity = 0.2,
  covariate_symbol = "circle open",
  covariate_symbol_size = 10,
  legend_position = "BR",
  logger = NULL
)
```

## Arguments

- model:

  Output from
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- regression_data:

  Output from
  [`baseline_regression()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_regression.md)
  or
  [`covariate_regression()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_regression.md)

- comparators:

  Vector of treatments to plot in colour. Cannot include the
  `reference_treatment` used in the model.

- include_covariate:

  logical. Whether the value of the covariate should be plotted as a
  vertical line. Defaults to `FALSE`.

- include_ghosts:

  logical. Whether the other comparator studies should be plotted in
  grey in the background of the plot. Defaults to `FALSE`.

- include_extrapolation:

  logical. Whether the regression lines should be extrapolated beyond
  the range of the given data as dashed lines. Defaults to `FALSE`.

- include_credible:

  logical. Whether the credible regions should be plotted for the
  specified comparators. These will be partially transparent regions.
  Defaults to `FALSE`.

- credible_opacity:

  numeric. The opacity of the credible regions. Can be any value between
  `0` and `1`, inclusive. Defaults to `0.2`.

- covariate_symbol:

  character. The selected symbol for displaying covariates. Defaults to
  `circle open`. Possible values are:

  cross

  :   Crosses

  circle_open

  :   Open circles

  none

  :   No symbols in which case only the plot of direct evidence is
      produced

- covariate_symbol_size:

  numeric. Size of the covariate symbols. Defaults to `10`.

- legend_position:

  character. The position of the legend. Defaults to `BR`. Possible
  values are:

  BR

  :   Bottom-right of the plot area

  BL

  :   Bottom-left of the plot area

  TR

  :   Top-right of the plot area

  TL

  :   Top-left of the plot area

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

html. Contains the svg string to generate the plot. This will display
the plot when at the end of a quarto or rmarkdown chunk. To view in the
viewer panel of Rstudio, use
[`htmltools::browsable()`](https://rstudio.github.io/htmltools/reference/browsable.html).
The output can be saved using
[`write_plot()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/write_plot.md).

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

# n_iter, max_iter and check_iter are set low to run quickly, but should
# be left as the default values in real use

fitted_baseline_model <- baseline_model(configured_data = configured_data,
                                        regressor_type = "shared",
                                        n_iter = 120,
                                        max_iter = 120,
                                        check_iter = 10)

regression_data <- baseline_regression(model = fitted_baseline_model,
                                       configured_data = configured_data)

metaregression_plot(model = fitted_baseline_model,
                    configured_data = configured_data,
                    regression_data = regression_data,
                    comparators = c("the_Younger", "the_Little"))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="1 1 1152 788">
#>   <g class="svglite">
#>     <defs>
#>       <style type="text/css"><![CDATA[
#>     .svglite line, .svglite polyline, .svglite polygon, .svglite path, .svglite rect, .svglite circle {
#>       fill: none;
#>       stroke: #000000;
#>       stroke-linecap: round;
#>       stroke-linejoin: round;
#>       stroke-miterlimit: 10.00;
#>     }
#>     .svglite text {
#>       white-space: pre;
#>     }
#>     .svglite g.glyphgroup path {
#>       fill: inherit;
#>       stroke: none;
#>     }
#>   ]]></style>
#>     </defs>
#>     <rect width="1152.00" height="792.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw">
#>         <rect x="0.00" y="0.00" width="1152.00" height="792.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#>       <rect x="0.00" y="0.00000000000011" width="1152.00" height="792.00" style="stroke-width: 1.07; stroke: #FFFFFF; fill: #FFFFFF;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNzYuMjl8MTE0MS4wNHwxMC45Nnw3NTAuMDk=">
#>         <rect x="76.29" y="10.96" width="1064.75" height="739.13"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNzYuMjl8MTE0MS4wNHwxMC45Nnw3NTAuMDk=)">
#> </g>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNS40OHwxMTQ2LjUyfDUuNDh8MTYyLjA3">
#>         <rect x="5.48" y="5.48" width="1141.04" height="156.59"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNS40OHwxMTQ2LjUyfDUuNDh8MTYyLjA3)">
#>       <rect x="5.48" y="5.48" width="1141.04" height="156.59" style="stroke-width: 1.07; stroke: none; fill: #FFFFFF;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNS40OHwxMTQ2LjUyfDE2Mi4wN3w3ODYuNTI=">
#>         <rect x="5.48" y="162.07" width="1141.04" height="624.45"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNS40OHwxMTQ2LjUyfDE2Mi4wN3w3ODYuNTI=)">
#>       <rect x="5.48" y="162.07" width="1141.04" height="624.45" style="stroke-width: 1.07; stroke: none; fill: #FFFFFF;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNzYuMjl8MTE0MS4wNHwxMC45NnwxNTYuNTk=">
#>         <rect x="76.29" y="10.96" width="1064.75" height="145.63"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNzYuMjl8MTE0MS4wNHwxMC45NnwxNTYuNTk=)">
#>       <rect x="76.29" y="10.96" width="1064.75" height="145.63" style="stroke-width: 4.27; stroke: #777777;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#>       <text transform="translate(20.59,83.78) rotate(-90)" text-anchor="middle" style="font-size: 14.00px; font-family: &quot;Liberation Sans&quot;;" textLength="45.89px" lengthAdjust="spacingAndGlyphs">Indirect</text>
#>       <text transform="translate(35.71,83.78) rotate(-90)" text-anchor="middle" style="font-size: 14.00px; font-family: &quot;Liberation Sans&quot;;" textLength="57.58px" lengthAdjust="spacingAndGlyphs">Evidence</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNzYuMjl8MTE0MS4wNHwxNjcuNTV8NzUwLjA5">
#>         <rect x="76.29" y="167.55" width="1064.75" height="582.54"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNzYuMjl8MTE0MS4wNHwxNjcuNTV8NzUwLjA5)">
#>       <circle cx="1055.74" cy="194.03" r="11.73" style="stroke-width: 2.13; stroke: #BB0000;"/>
#>       <circle cx="317.61" cy="723.61" r="11.73" style="stroke-width: 2.13; stroke: #BBA000;"/>
#>       <circle cx="502.14" cy="194.03" r="11.73" style="stroke-width: 2.13; stroke: #BBA000;"/>
#>       <line x1="1055.74" y1="386.28" x2="1055.74" y2="386.28" style="stroke-width: 2.56; stroke: #BB0000; stroke-linecap: butt;"/>
#>       <line x1="317.61" y1="503.22" x2="502.14" y2="661.01" style="stroke-width: 2.56; stroke: #BBA000; stroke-linecap: butt;"/>
#>       <rect x="76.29" y="167.55" width="1064.75" height="582.54" style="stroke-width: 4.27; stroke: #777777;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHwxMTUyLjAwfDAuMDB8NzkyLjAw)">
#>       <text x="71.36" y="655.53" text-anchor="end" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">11.25</text>
#>       <text x="71.36" y="535.17" text-anchor="end" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">11.50</text>
#>       <text x="71.36" y="414.81" text-anchor="end" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">11.75</text>
#>       <text x="71.36" y="294.45" text-anchor="end" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">12.00</text>
#>       <text x="71.36" y="174.09" text-anchor="end" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">12.25</text>
#>       <text x="133.08" y="763.28" text-anchor="middle" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="468.59" y="763.28" text-anchor="middle" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="804.10" y="763.28" text-anchor="middle" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <text x="1139.62" y="763.28" text-anchor="middle" style="font-size: 12.00px;fill: #4D4D4D; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">6</text>
#>       <text x="608.67" y="778.14" text-anchor="middle" style="font-size: 14.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.55px" lengthAdjust="spacingAndGlyphs">Covariate Value</text>
#>       <text transform="translate(20.59,458.82) rotate(-90)" text-anchor="middle" style="font-size: 14.00px; font-family: &quot;Liberation Sans&quot;;" textLength="205.09px" lengthAdjust="spacingAndGlyphs">Relative Effect vs the Great (MD)</text>
#>       <rect x="1031.63" y="676.60" width="98.76" height="61.84" style="stroke-width: 1.07; fill: #FFFFFF; fill-opacity: 0.67;"/>
#>       <text x="1037.61" y="691.28" style="font-size: 11.00px; font-family: &quot;Liberation Sans&quot;;" textLength="49.75px" lengthAdjust="spacingAndGlyphs">Treatment</text>
#>       <line x1="1039.34" y1="706.54" x2="1053.16" y2="706.54" style="stroke-width: 2.56; stroke: #BB0000; stroke-linecap: butt;"/>
#>       <line x1="1039.34" y1="723.82" x2="1053.16" y2="723.82" style="stroke-width: 2.56; stroke: #BBA000; stroke-linecap: butt;"/>
#>       <text x="1060.37" y="710.67" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="45.34px" lengthAdjust="spacingAndGlyphs">the Little</text>
#>       <text x="1060.37" y="727.95" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.05px" lengthAdjust="spacingAndGlyphs">the Younger</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
