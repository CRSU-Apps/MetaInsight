# Produce a forest plot of pairwise comparisons, grouped by treatment pairs. If risk of bias data was loaded these are also included.

Produce a forest plot of pairwise comparisons, grouped by treatment
pairs. If risk of bias data was loaded these are also included.

## Usage

``` r
summary_study(
  configured_data,
  plot_area_width = 6,
  colourblind = FALSE,
  x_min = NULL,
  x_max = NULL,
  interactive = FALSE,
  logger = NULL
)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- plot_area_width:

  numeric. The width of the plot area containing the treatment effects
  in inches. Defaults to `6`.

- colourblind:

  logical. Whether to use a colourblind-friendly palette. Defaults to
  `FALSE`

- x_min:

  numeric. Minimum value for the x-axis. Defaults to `NULL`. For binary
  outcomes values should be wrapped in
  [`log()`](https://rdrr.io/r/base/Log.html)

- x_max:

  numeric. Maximum value for the x-axis. Defaults to `NULL`. For binary
  outcomes values should be wrapped in
  [`log()`](https://rdrr.io/r/base/Log.html)

- interactive:

  logical. Whether the plot should be altered for preparation into an
  interactive interface. Defaults to `FALSE`

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

summary_study(configured_data = configured_data)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="49 -8 687 289">
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
#>     <rect width="750.24" height="316.80" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=">
#>         <rect x="0.00" y="0.00" width="750.24" height="316.80"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="534.24" y="270.72" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="85.81px" lengthAdjust="spacingAndGlyphs">Mean difference</text>
#>       <text x="534.24" y="11.52" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="385.52px" lengthAdjust="spacingAndGlyphs">Individual study results (with selected studies excluded)</text>
#>       <text x="534.24" y="28.80" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="237.53px" lengthAdjust="spacingAndGlyphs"> grouped by treatment comparison</text>
#>       <line x1="375.84" y1="216.00" x2="680.90" y2="216.00" style="stroke-width: 0.75;"/>
#>       <line x1="387.57" y1="216.00" x2="387.57" y2="223.20" style="stroke-width: 0.75;"/>
#>       <line x1="534.24" y1="216.00" x2="534.24" y2="223.20" style="stroke-width: 0.75;"/>
#>       <line x1="680.90" y1="216.00" x2="680.90" y2="223.20" style="stroke-width: 0.75;"/>
#>       <text x="387.57" y="241.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">10</text>
#>       <text x="534.24" y="241.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">15</text>
#>       <text x="680.90" y="241.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="13.34px" lengthAdjust="spacingAndGlyphs">20</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=">
#>         <rect x="375.84" y="57.60" width="316.80" height="158.40"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polyline points="94.24,216.00 94.24,81.97 " style="stroke-width: 0.75; stroke-dasharray: 4.00,4.00;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="299.47" y="59.29" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="91.34px" lengthAdjust="spacingAndGlyphs">Mean difference</text>
#>       <text x="299.47" y="73.91" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="47.34px" lengthAdjust="spacingAndGlyphs">(95% CI)</text>
#>       <text x="57.60" y="181.14" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="132.67px" lengthAdjust="spacingAndGlyphs">the_Little vs. the_Great</text>
#>       <text x="72.00" y="195.76" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.69px" lengthAdjust="spacingAndGlyphs">Minerva</text>
#>     </g>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polygon points="449.40,191.63 452.10,188.93 454.80,191.63 452.10,194.33 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="439.33,191.63 464.88,191.63 " style="stroke-width: 0.75;"/>
#>       <polyline points="439.33,195.29 439.33,187.98 " style="stroke-width: 0.75;"/>
#>       <polyline points="464.88,195.29 464.88,187.98 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="237.50" y="195.76" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="107.14px" lengthAdjust="spacingAndGlyphs">12.20 (11.76, 12.64)</text>
#>       <text x="57.60" y="132.40" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="149.36px" lengthAdjust="spacingAndGlyphs">the_Butcher vs. the_Great</text>
#>       <text x="72.00" y="147.02" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="47.36px" lengthAdjust="spacingAndGlyphs">Justinian</text>
#>       <text x="72.00" y="159.21" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="26.02px" lengthAdjust="spacingAndGlyphs">Nero</text>
#>     </g>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polygon points="449.40,142.89 452.10,140.19 454.80,142.89 452.10,145.59 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="438.64,142.89 465.56,142.89 " style="stroke-width: 0.75;"/>
#>       <polyline points="438.64,146.55 438.64,139.24 " style="stroke-width: 0.75;"/>
#>       <polyline points="465.56,146.55 465.56,139.24 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="237.50" y="147.02" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="107.14px" lengthAdjust="spacingAndGlyphs">12.20 (11.74, 12.66)</text>
#>     </g>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polygon points="449.40,155.08 452.10,152.38 454.80,155.08 452.10,157.78 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="439.00,155.08 465.21,155.08 " style="stroke-width: 0.75;"/>
#>       <polyline points="439.00,158.73 439.00,151.42 " style="stroke-width: 0.75;"/>
#>       <polyline points="465.21,158.73 465.21,151.42 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="237.50" y="159.21" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="107.14px" lengthAdjust="spacingAndGlyphs">12.20 (11.75, 12.65)</text>
#>       <text x="57.60" y="83.66" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="151.78px" lengthAdjust="spacingAndGlyphs">the_Younger vs. the_Great</text>
#>       <text x="72.00" y="98.28" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.03px" lengthAdjust="spacingAndGlyphs">Constantine</text>
#>       <text x="72.00" y="110.47" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">Leo</text>
#>     </g>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polygon points="417.14,94.15 419.84,91.45 422.54,94.15 419.84,96.85 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="405.11,94.15 434.56,94.15 " style="stroke-width: 0.75;"/>
#>       <polyline points="405.11,97.81 405.11,90.50 " style="stroke-width: 0.75;"/>
#>       <polyline points="434.56,97.81 434.56,90.50 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="237.50" y="98.28" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.25px" lengthAdjust="spacingAndGlyphs">11.10 (10.60, 11.60)</text>
#>     </g>
#>     <g clip-path="url(#cpMzc1Ljg0fDY5Mi42NHw1Ny42MHwyMTYuMDA=)">
#>       <polygon points="449.40,106.34 452.10,103.64 454.80,106.34 452.10,109.04 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="438.05,106.34 466.15,106.34 " style="stroke-width: 0.75;"/>
#>       <polyline points="438.05,109.99 438.05,102.68 " style="stroke-width: 0.75;"/>
#>       <polyline points="466.15,109.99 466.15,102.68 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3NTAuMjR8MC4wMHwzMTYuODA=)">
#>       <text x="237.50" y="110.47" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="107.14px" lengthAdjust="spacingAndGlyphs">12.20 (11.72, 12.68)</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
