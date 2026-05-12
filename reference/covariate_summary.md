# Summarise covariate data

Produce a plot summarising the covariate value for each study arm

## Usage

``` r
covariate_summary(configured_data, logger = NULL)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

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

covariate_summary(configured_data)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="77 9 417 372">
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
#>     <rect width="547.48" height="460.80" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw1NDcuNDh8MC4wMHw0NjAuODA=">
#>         <rect x="0.00" y="0.00" width="547.48" height="460.80"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw1NDcuNDh8MC4wMHw0NjAuODA=)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTczLjA4fDQ4OS44OHw1Ny42MHwyODguMDA=">
#>         <rect x="173.08" y="57.60" width="316.80" height="230.40"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTczLjA4fDQ4OS44OHw1Ny42MHwyODguMDA=)">
#>       <polygon points="475.45,275.20 478.15,272.50 480.85,275.20 478.15,277.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="402.11,262.40 404.81,259.70 407.51,262.40 404.81,265.10 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="182.11,224.00 184.81,221.30 187.51,224.00 184.81,226.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="475.45,185.60 478.15,182.90 480.85,185.60 478.15,188.30 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="328.78,172.80 331.48,170.10 334.18,172.80 331.48,175.50 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="402.11,160.00 404.81,157.30 407.51,160.00 404.81,162.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="182.11,147.20 184.81,144.50 187.51,147.20 184.81,149.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="255.45,134.40 258.15,131.70 260.85,134.40 258.15,137.10 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="328.78,96.00 331.48,93.30 334.18,96.00 331.48,98.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polygon points="255.45,83.20 258.15,80.50 260.85,83.20 258.15,85.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw1NDcuNDh8MC4wMHw0NjAuODA=)">
#>       <text x="331.48" y="342.72" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="21.34px" lengthAdjust="spacingAndGlyphs">Age</text>
#>       <text x="331.48" y="28.80" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="114.38px" lengthAdjust="spacingAndGlyphs">Covariate values</text>
#>       <text x="100.80" y="279.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.03px" lengthAdjust="spacingAndGlyphs">Constantine</text>
#>       <text x="100.80" y="266.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">Leo</text>
#>       <text x="100.80" y="228.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.69px" lengthAdjust="spacingAndGlyphs">Minerva</text>
#>       <text x="100.80" y="189.73" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.03px" lengthAdjust="spacingAndGlyphs">Constantine</text>
#>       <text x="100.80" y="176.93" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="47.36px" lengthAdjust="spacingAndGlyphs">Justinian</text>
#>       <text x="100.80" y="164.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">Leo</text>
#>       <text x="100.80" y="151.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.69px" lengthAdjust="spacingAndGlyphs">Minerva</text>
#>       <text x="100.80" y="138.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="26.02px" lengthAdjust="spacingAndGlyphs">Nero</text>
#>       <text x="100.80" y="100.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="47.36px" lengthAdjust="spacingAndGlyphs">Justinian</text>
#>       <text x="100.80" y="87.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="26.02px" lengthAdjust="spacingAndGlyphs">Nero</text>
#>       <text x="86.40" y="251.17" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="72.44px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <text x="86.40" y="212.77" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="53.33px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <text x="86.40" y="123.17" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="56.02px" lengthAdjust="spacingAndGlyphs">the_Great</text>
#>       <text x="86.40" y="71.97" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="70.02px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>     </g>
#>     <g clip-path="url(#cpMTczLjA4fDQ4OS44OHw1Ny42MHwyODguMDA=)">
#>       <line x1="331.48" y1="288.00" x2="331.48" y2="57.60" style="stroke-width: 0.75; stroke-dasharray: 4.00,4.00;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw1NDcuNDh8MC4wMHw0NjAuODA=)">
#>       <line x1="184.81" y1="288.00" x2="478.15" y2="288.00" style="stroke-width: 0.75;"/>
#>       <line x1="184.81" y1="288.00" x2="184.81" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="258.15" y1="288.00" x2="258.15" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="331.48" y1="288.00" x2="331.48" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="404.81" y1="288.00" x2="404.81" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="478.15" y1="288.00" x2="478.15" y2="295.20" style="stroke-width: 0.75;"/>
#>       <text x="184.81" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">95</text>
#>       <text x="258.15" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">96</text>
#>       <text x="331.48" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">97</text>
#>       <text x="404.81" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">98</text>
#>       <text x="478.15" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">99</text>
#>       <text x="100.80" y="368.93" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="354.05px" lengthAdjust="spacingAndGlyphs">The plotted value is the same for all treatment arms across a study</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
