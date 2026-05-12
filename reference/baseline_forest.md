# Produce a forest plot for baseline risk models

Produce a forest plot for a baseline risk model using
[`gemtc::blobbogram()`](https://rdrr.io/pkg/gemtc/man/blobbogram.html)

## Usage

``` r
baseline_forest(
  model,
  xmin = NULL,
  xmax = NULL,
  title = "Baseline risk regression analysis",
  ranking = FALSE,
  logger = NULL
)
```

## Arguments

- model:

  Output produced by
  [`baseline_model()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/baseline_model.md)

- xmin:

  numeric. Minimum x-axis value. Default `NULL` in which case it is
  calculated internally

- xmax:

  numeric. Maximum x-axis value. Default `NULL` in which case it is
  calculated internally

- title:

  character. Title for the plot. Defaults to
  `Baseline risk regression analysis`

- ranking:

  logical. Whether the function is being used in `baseline_ranking`

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

baseline_forest(model = fitted_baseline_model)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="23 15 439 202">
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
#>     <rect width="460.80" height="285.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw0NjAuODB8MC4wMHwyODUuMDA=">
#>         <rect x="0.00" y="0.00" width="460.80" height="285.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw0NjAuODB8MC4wMHwyODUuMDA=)">
#>       <text x="382.77" y="94.79" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="140.48px" lengthAdjust="spacingAndGlyphs">Mean difference (95% CrI)</text>
#>       <text x="32.02" y="112.79" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.00px" lengthAdjust="spacingAndGlyphs">Compared with the_Great</text>
#>       <text x="32.02" y="130.79" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="67.61px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <polyline points="193.32,126.66 270.46,126.66 " style="stroke-width: 0.75;"/>
#>       <circle cx="261.80" cy="126.66" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="336.76" y="130.79" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="92.02px" lengthAdjust="spacingAndGlyphs">10.9 (-80.9, 22.5)</text>
#>       <text x="32.02" y="145.19" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.69px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <polyline points="251.76,141.06 289.78,141.06 " style="stroke-width: 0.75;"/>
#>       <circle cx="262.72" cy="141.06" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="336.76" y="145.19" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="92.02px" lengthAdjust="spacingAndGlyphs">12.1 (-2.55, 48.4)</text>
#>       <text x="32.02" y="159.59" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="48.69px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <polyline points="250.28,155.46 323.25,155.46 " style="stroke-width: 0.75;"/>
#>       <circle cx="262.95" cy="155.46" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="336.76" y="159.59" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="92.02px" lengthAdjust="spacingAndGlyphs">12.5 (-4.54, 93.3)</text>
#>       <polyline points="253.66,169.86 253.66,97.86 " style="stroke-width: 0.75;"/>
#>       <polyline points="186.53,162.66 328.26,162.66 " style="stroke-width: 0.75;"/>
#>       <polyline points="186.53,169.86 186.53,162.66 " style="stroke-width: 0.75;"/>
#>       <polyline points="328.26,169.86 328.26,162.66 " style="stroke-width: 0.75;"/>
#>       <text x="253.66" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="186.53" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="17.34px" lengthAdjust="spacingAndGlyphs">-90</text>
#>       <text x="328.26" y="181.19" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">100</text>
#>       <text x="244.80" y="34.47" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="227.16px" lengthAdjust="spacingAndGlyphs">Baseline risk regression analysis</text>
#>       <text x="244.80" y="53.09" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="219.84px" lengthAdjust="spacingAndGlyphs">Between-study standard deviation: 11.03 </text>
#>       <text x="244.80" y="67.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="184.72px" lengthAdjust="spacingAndGlyphs"> 95% credible interval: 0.45 , 60.48</text>
#>       <text x="244.80" y="206.56" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="173.14px" lengthAdjust="spacingAndGlyphs">Value for baseline risk set at 3.3 </text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
