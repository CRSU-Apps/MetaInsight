# Produce a frequentist forest plot

Produce an annotated frequentist forest plot using
[`meta::forest()`](https://wviechtb.github.io/metafor/reference/forest.html)

## Usage

``` r
freq_forest(
  configured_data,
  xmin = NULL,
  xmax = NULL,
  title = "",
  logger = NULL
)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- xmin:

  numeric. Minimum x-axis value. Default `NULL` in which case it is
  calculated internally

- xmax:

  numeric. Maximum x-axis value. Default `NULL` in which case it is
  calculated internally

- title:

  character. Title for the plot.

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

freq_forest(configured_data = configured_data)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="31 24 377 191">
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
#>     <rect width="439.20" height="285.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw0MzkuMjB8MC4wMHwyODUuMDA=">
#>         <rect x="0.00" y="0.00" width="439.20" height="285.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw0MzkuMjB8MC4wMHwyODUuMDA=)">
#>       <text x="40.57" y="117.83" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="57.36px" lengthAdjust="spacingAndGlyphs">Treatment</text>
#>       <text x="40.57" y="146.63" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.69px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <text x="40.57" y="161.03" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="48.69px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <text x="40.57" y="175.43" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="67.61px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <polygon points="113.85,178.50 283.93,178.50 283.93,135.30 113.85,135.30 " style="stroke-width: 0.75; stroke: none;"/>
#>       <polyline points="113.85,178.50 283.93,178.50 " style="stroke-width: 0.75;"/>
#>       <line x1="113.85" y1="178.50" x2="113.85" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="147.87" y1="178.50" x2="147.87" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="181.88" y1="178.50" x2="181.88" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="215.90" y1="178.50" x2="215.90" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="249.91" y1="178.50" x2="249.91" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="283.93" y1="178.50" x2="283.93" y2="185.70" style="stroke-width: 0.75;"/>
#>       <text x="113.85" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">10</text>
#>       <text x="147.87" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">12</text>
#>       <text x="181.88" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">14</text>
#>       <text x="215.90" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">16</text>
#>       <text x="249.91" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">18</text>
#>       <text x="283.93" y="204.23" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">20</text>
#>       <polyline points="113.85,178.50 283.93,178.50 " style="stroke-width: 0.75;"/>
#>       <line x1="113.85" y1="178.50" x2="113.85" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="147.87" y1="178.50" x2="147.87" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="181.88" y1="178.50" x2="181.88" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="215.90" y1="178.50" x2="215.90" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="249.91" y1="178.50" x2="249.91" y2="185.70" style="stroke-width: 0.75;"/>
#>       <line x1="283.93" y1="178.50" x2="283.93" y2="185.70" style="stroke-width: 0.75;"/>
#>       <text x="198.89" y="103.43" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="189.06px" lengthAdjust="spacingAndGlyphs">Comparison: other vs 'the_Great'</text>
#>       <text x="198.89" y="117.83" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="137.33px" lengthAdjust="spacingAndGlyphs">(Random Effects Model)</text>
#>       <rect x="146.08" y="137.32" width="10.37" height="10.37" style="stroke-width: 0.75; stroke: #BEBEBE; fill: #BEBEBE;"/>
#>       <polyline points="151.27,143.94 151.27,141.06 " style="stroke-width: 0.75;"/>
#>       <polyline points="138.93,142.50 163.61,142.50 " style="stroke-width: 0.75;"/>
#>       <rect x="146.08" y="151.72" width="10.37" height="10.37" style="stroke-width: 0.75; stroke: #BEBEBE; fill: #BEBEBE;"/>
#>       <polyline points="151.27,158.34 151.27,155.46 " style="stroke-width: 0.75;"/>
#>       <polyline points="133.94,156.90 168.59,156.90 " style="stroke-width: 0.75;"/>
#>       <rect x="136.83" y="166.12" width="10.37" height="10.37" style="stroke-width: 0.75; stroke: #BEBEBE; fill: #BEBEBE;"/>
#>       <polyline points="142.01,172.74 142.01,169.86 " style="stroke-width: 0.75;"/>
#>       <polyline points="129.46,171.30 154.56,171.30 " style="stroke-width: 0.75;"/>
#>       <text x="319.61" y="117.83" text-anchor="end" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="18.67px" lengthAdjust="spacingAndGlyphs">MD</text>
#>       <text x="319.61" y="146.63" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">12.20</text>
#>       <text x="319.61" y="161.03" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">12.20</text>
#>       <text x="319.61" y="175.43" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">11.66</text>
#>       <text x="398.63" y="117.83" text-anchor="end" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="39.34px" lengthAdjust="spacingAndGlyphs">95% CI</text>
#>       <text x="398.63" y="146.63" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="72.45px" lengthAdjust="spacingAndGlyphs">[11.47; 12.93]</text>
#>       <text x="398.63" y="161.03" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="72.45px" lengthAdjust="spacingAndGlyphs">[11.18; 13.22]</text>
#>       <text x="398.63" y="175.43" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="73.34px" lengthAdjust="spacingAndGlyphs">[10.92; 12.39]</text>
#>       <text x="219.60" y="42.29" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="210.73px" lengthAdjust="spacingAndGlyphs">Between-study standard deviation: 0.47</text>
#>       <text x="219.60" y="59.57" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="250.02px" lengthAdjust="spacingAndGlyphs"> Number of studies: 5, Number of treatments: 4</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
