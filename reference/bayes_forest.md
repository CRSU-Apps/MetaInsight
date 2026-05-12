# Bayesian forest plot

Produce a Bayesian forest plot with
[`gemtc::forest()`](https://rdrr.io/pkg/gemtc/man/blobbogram.html)

## Usage

``` r
bayes_forest(
  model,
  xmin = NULL,
  xmax = NULL,
  title = "",
  ranking = FALSE,
  logger = NULL
)

covariate_forest(...)
```

## Arguments

- model:

  list. Object created by
  [`bayes_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_model.md)
  or
  [`covariate_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/covariate_model.md)

- xmin:

  numeric. Minimum x-axis value. Default `NULL` in which case it is
  calculated internally

- xmax:

  numeric. Maximum x-axis value. Default `NULL` in which case it is
  calculated internally

- title:

  character. Title for the plot. Default is no title

- ranking:

  logical. Whether the function is being used in `bayes_ranking`

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- ...:

  Parameters passed to `bayes_forest()`

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

# n_adapt and n_iter are set low to run quickly, but should be left as the
# default values in real use

fitted_bayes_model <- bayes_model(configured_data = configured_data,
                                  n_adapt = 100,
                                  n_iter = 100)

bayes_forest(model = fitted_bayes_model)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="57 35 439 157">
#>   <g class="svglite">
#>     <defs>
#>       <style type="text/css"><![CDATA[
#>     @import url("https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap");
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
#>     <rect width="525.60" height="285.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw1MjUuNjB8MC4wMHwyODUuMDA=">
#>         <rect x="0.00" y="0.00" width="525.60" height="285.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw1MjUuNjB8MC4wMHwyODUuMDA=)">
#>       <text x="415.17" y="94.79" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="142.48px" lengthAdjust="spacingAndGlyphs">Mean Difference (95% CrI)</text>
#>       <text x="66.42" y="112.79" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.00px" lengthAdjust="spacingAndGlyphs">Compared with the_Great</text>
#>       <text x="66.42" y="130.79" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.69px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <polyline points="282.27,126.66 330.15,126.66 " style="stroke-width: 0.75;"/>
#>       <circle cx="306.91" cy="126.66" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="371.16" y="130.79" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">12.1 (8.66, 15.4)</text>
#>       <text x="66.42" y="145.19" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="48.69px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <polyline points="269.97,141.06 336.39,141.06 " style="stroke-width: 0.75;"/>
#>       <circle cx="306.66" cy="141.06" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="371.16" y="145.19" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">12.1 (6.92, 16.3)</text>
#>       <text x="66.42" y="159.59" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="67.61px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <polyline points="278.52,155.46 324.96,155.46 " style="stroke-width: 0.75;"/>
#>       <circle cx="303.46" cy="155.46" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="371.16" y="159.59" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="87.12px" lengthAdjust="spacingAndGlyphs">11.6 (8.13, 14.7)</text>
#>       <polyline points="220.93,169.86 220.93,97.86 " style="stroke-width: 0.75;"/>
#>       <polyline points="220.93,162.66 362.66,162.66 " style="stroke-width: 0.75;"/>
#>       <polyline points="220.93,169.86 220.93,162.66 " style="stroke-width: 0.75;"/>
#>       <polyline points="362.66,169.86 362.66,162.66 " style="stroke-width: 0.75;"/>
#>       <text x="220.93" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="220.93" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="362.66" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">20</text>
#>       <text x="277.20" y="53.09" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="214.06px" lengthAdjust="spacingAndGlyphs">Between-study standard deviation: 1.62 </text>
#>       <text x="277.20" y="67.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="171.38px" lengthAdjust="spacingAndGlyphs"> 95% credible interval: 0.2 , 6.05</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
