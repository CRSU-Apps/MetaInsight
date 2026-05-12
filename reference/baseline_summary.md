# Summarise baseline risk

Produce a plot summarising baselink risk for each study arm

## Usage

``` r
baseline_summary(configured_data, logger = NULL)
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

## Examples

``` r
configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
configured_data <- readRDS(configured_data_path)

baseline_summary(configured_data)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="48 14 544 406">
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
#>     <rect width="648.50" height="460.80" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=">
#>         <rect x="0.00" y="0.00" width="648.50" height="460.80"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="432.50" y="342.72" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="68.03px" lengthAdjust="spacingAndGlyphs">Baseline risk</text>
#>       <text x="432.50" y="33.75" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="227.14px" lengthAdjust="spacingAndGlyphs">Baseline risk based on the_Great</text>
#>       <line x1="274.10" y1="288.00" x2="579.16" y2="288.00" style="stroke-width: 0.75;"/>
#>       <line x1="330.11" y1="288.00" x2="330.11" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="413.13" y1="288.00" x2="413.13" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="496.14" y1="288.00" x2="496.14" y2="295.20" style="stroke-width: 0.75;"/>
#>       <line x1="579.16" y1="288.00" x2="579.16" y2="295.20" style="stroke-width: 0.75;"/>
#>       <text x="330.11" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="16.67px" lengthAdjust="spacingAndGlyphs">1.5</text>
#>       <text x="413.13" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">3</text>
#>       <text x="496.14" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="16.67px" lengthAdjust="spacingAndGlyphs">4.5</text>
#>       <text x="579.16" y="313.92" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">6</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=">
#>         <rect x="274.10" y="57.60" width="316.80" height="230.40"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <line x1="429.73" y1="288.00" x2="429.73" y2="57.60" style="stroke-width: 0.75; stroke-dasharray: 4.00,4.00;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="209.19" y="46.37" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="74.02px" lengthAdjust="spacingAndGlyphs">Baseline risk</text>
#>       <text x="209.19" y="61.73" text-anchor="middle" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="47.34px" lengthAdjust="spacingAndGlyphs">(95% CI)</text>
#>       <text x="57.60" y="251.17" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="72.44px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <text x="72.00" y="279.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.03px" lengthAdjust="spacingAndGlyphs">Constantine</text>
#>       <text x="72.00" y="266.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">Leo</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="305.27,275.20 307.97,272.50 310.67,275.20 307.97,277.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="288.16,275.20 327.77,275.20 " style="stroke-width: 0.75;"/>
#>       <polyline points="288.16,279.04 288.16,271.36 " style="stroke-width: 0.75;"/>
#>       <polyline points="327.77,279.04 327.77,271.36 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="279.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">1.10 (0.74, 1.46)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="366.15,262.40 368.85,259.70 371.55,262.40 368.85,265.10 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="349.97,262.40 387.73,262.40 " style="stroke-width: 0.75;"/>
#>       <polyline points="349.97,266.24 349.97,258.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="387.73,266.24 387.73,258.56 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="266.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">2.20 (1.86, 2.54)</text>
#>       <text x="57.60" y="212.77" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="53.33px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <text x="72.00" y="228.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.69px" lengthAdjust="spacingAndGlyphs">Minerva</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="548.79,224.00 551.49,221.30 554.19,224.00 551.49,226.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="534.34,224.00 568.64,224.00 " style="stroke-width: 0.75;"/>
#>       <polyline points="534.34,227.84 534.34,220.16 " style="stroke-width: 0.75;"/>
#>       <polyline points="568.64,227.84 568.64,220.16 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="228.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">5.50 (5.19, 5.81)</text>
#>       <text x="57.60" y="123.17" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="56.02px" lengthAdjust="spacingAndGlyphs">the_Great</text>
#>       <text x="72.00" y="189.73" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.03px" lengthAdjust="spacingAndGlyphs">Constantine</text>
#>       <text x="72.00" y="176.93" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="47.36px" lengthAdjust="spacingAndGlyphs">Justinian</text>
#>       <text x="72.00" y="164.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="20.02px" lengthAdjust="spacingAndGlyphs">Leo</text>
#>       <text x="72.00" y="151.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.69px" lengthAdjust="spacingAndGlyphs">Minerva</text>
#>       <text x="72.00" y="138.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="26.02px" lengthAdjust="spacingAndGlyphs">Nero</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="305.27,185.60 307.97,182.90 310.67,185.60 307.97,188.30 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="288.16,185.60 327.77,185.60 " style="stroke-width: 0.75;"/>
#>       <polyline points="288.16,189.44 288.16,181.76 " style="stroke-width: 0.75;"/>
#>       <polyline points="327.77,189.44 327.77,181.76 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="189.73" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">1.10 (0.74, 1.46)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="427.03,172.80 429.73,170.10 432.43,172.80 429.73,175.50 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="411.65,172.80 447.81,172.80 " style="stroke-width: 0.75;"/>
#>       <polyline points="411.65,176.64 411.65,168.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="447.81,176.64 447.81,168.96 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="176.93" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">3.30 (2.97, 3.63)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="366.15,160.00 368.85,157.30 371.55,160.00 368.85,162.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="349.97,160.00 387.73,160.00 " style="stroke-width: 0.75;"/>
#>       <polyline points="349.97,163.84 349.97,156.16 " style="stroke-width: 0.75;"/>
#>       <polyline points="387.73,163.84 387.73,156.16 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="164.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">2.20 (1.86, 2.54)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="548.79,147.20 551.49,144.50 554.19,147.20 551.49,149.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="534.34,147.20 568.64,147.20 " style="stroke-width: 0.75;"/>
#>       <polyline points="534.34,151.04 534.34,143.36 " style="stroke-width: 0.75;"/>
#>       <polyline points="568.64,151.04 568.64,143.36 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="151.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">5.50 (5.19, 5.81)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="487.91,134.40 490.61,131.70 493.31,134.40 490.61,137.10 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="473.01,134.40 508.21,134.40 " style="stroke-width: 0.75;"/>
#>       <polyline points="473.01,138.24 473.01,130.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="508.21,138.24 508.21,130.56 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="138.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">4.40 (4.08, 4.72)</text>
#>       <text x="57.60" y="71.97" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="70.02px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <text x="72.00" y="100.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="47.36px" lengthAdjust="spacingAndGlyphs">Justinian</text>
#>       <text x="72.00" y="87.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="26.02px" lengthAdjust="spacingAndGlyphs">Nero</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="427.03,96.00 429.73,93.30 432.43,96.00 429.73,98.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="411.65,96.00 447.81,96.00 " style="stroke-width: 0.75;"/>
#>       <polyline points="411.65,99.84 411.65,92.16 " style="stroke-width: 0.75;"/>
#>       <polyline points="447.81,99.84 447.81,92.16 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="100.13" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">3.30 (2.97, 3.63)</text>
#>     </g>
#>     <g clip-path="url(#cpMjc0LjEwfDU5MC45MHw1Ny42MHwyODguMDA=)">
#>       <polygon points="487.91,83.20 490.61,80.50 493.31,83.20 490.61,85.90 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="473.01,83.20 508.21,83.20 " style="stroke-width: 0.75;"/>
#>       <polyline points="473.01,87.04 473.01,79.36 " style="stroke-width: 0.75;"/>
#>       <polyline points="508.21,87.04 508.21,79.36 " style="stroke-width: 0.75;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw2NDguNTB8MC4wMHw0NjAuODA=)">
#>       <text x="158.68" y="87.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="88.02px" lengthAdjust="spacingAndGlyphs">4.40 (4.08, 4.72)</text>
#>       <text x="72.00" y="368.93" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="382.08px" lengthAdjust="spacingAndGlyphs">The plotted baseline risk value is the same for all treatment arms across</text>
#>       <text x="72.00" y="381.73" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="311.38px" lengthAdjust="spacingAndGlyphs">a study and is the outcome in the reference treatment arm.</text>
#>       <text x="72.00" y="394.53" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="361.17px" lengthAdjust="spacingAndGlyphs">Values for studies without a reference treatment arm are not plotted.</text>
#>       <text x="72.00" y="407.33" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="209.02px" lengthAdjust="spacingAndGlyphs">Error bars: mean +/- 1.96 * SD / sqrt(N)</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
