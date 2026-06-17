# Bayesian nodesplitting forest plot

Produce a forest plot from nodesplitting results

## Usage

``` r
bayes_nodesplit_plot(nodesplit, main_analysis = TRUE, logger = NULL)
```

## Arguments

- nodesplit:

  `mtc.nodesplit` object produced by
  [`bayes_nodesplit()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/bayes_nodesplit.md)

- main_analysis:

  logical. Whether the analysis is the main or sensitivity analysis.
  Default `TRUE`.

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
# \donttest{
nodesplit_path <- system.file("extdata", "continuous_nodesplit.csv", package = "metainsight")
loaded_data <- setup_load(data_path = nodesplit_path,
                          outcome = "continuous")

configured_data <- setup_configure(loaded_data = loaded_data,
                                   reference_treatment = "Placebo",
                                   effects = "random",
                                   outcome_measure = "MD",
                                   ranking_option = "good",
                                   seed = 123)

nodesplit_model <- bayes_nodesplit(configured_data,
                                   n_adapt = 100,
                                   n_iter = 100)

bayes_nodesplit_plot(nodesplit_model)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="60 6 466 630">
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
#>     <rect width="576.00" height="720.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw1NzYuMDB8MC4wMHw3MjAuMDA=">
#>         <rect x="0.00" y="0.00" width="576.00" height="720.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw1NzYuMDB8MC4wMHw3MjAuMDA=)">
#>       <text x="69.36" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.67px" lengthAdjust="spacingAndGlyphs">Study</text>
#>       <text x="151.53" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="40.69px" lengthAdjust="spacingAndGlyphs">P-value</text>
#>       <text x="445.29" y="85.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="142.48px" lengthAdjust="spacingAndGlyphs">Mean Difference (95% CrI)</text>
#>       <text x="69.36" y="103.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="135.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Placebo</text>
#>       <text x="69.36" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="269.20,117.36 281.27,117.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="274.81" cy="117.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.68 (-2.00, -1.31)</text>
#>       <text x="69.36" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.135</text>
#>       <polyline points="269.57,131.76 319.87,131.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="295.06" cy="131.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.536 (-1.98, 0.864)</text>
#>       <text x="69.36" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="269.71,146.16 280.55,146.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="275.30" cy="146.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.65 (-1.97, -1.36)</text>
#>       <text x="69.36" y="168.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="108.00px" lengthAdjust="spacingAndGlyphs">Orlistat vs Placebo</text>
#>       <text x="69.36" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="274.61,182.16 294.06,182.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="284.18" cy="182.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.15 (-1.69, -0.593)</text>
#>       <text x="69.36" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.655</text>
#>       <polyline points="272.07,196.56 307.23,196.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.86" cy="196.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.943 (-1.83, 0.150)</text>
#>       <text x="69.36" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="279.36,210.96 292.36,210.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="285.15" cy="210.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.10 (-1.42, -0.689)</text>
#>       <text x="69.36" y="233.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="131.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Orlistat</text>
#>       <text x="69.36" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="285.28,246.96 308.03,246.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="297.69" cy="246.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.388 (-1.09, 0.196)</text>
#>       <text x="69.36" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.445</text>
#>       <polyline points="277.03,261.36 302.89,261.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="290.84" cy="261.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="120.03px" lengthAdjust="spacingAndGlyphs">-0.774 (-1.55, -0.0945)</text>
#>       <text x="69.36" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="286.37,275.76 302.17,275.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="294.67" cy="275.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="113.36px" lengthAdjust="spacingAndGlyphs">-0.559 (-1.03, -0.135)</text>
#>       <text x="69.36" y="297.89" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="119.31px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Orlistat</text>
#>       <text x="69.36" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="261.12,311.76 296.46,311.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="278.12" cy="311.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.49 (-2.45, -0.458)</text>
#>       <text x="69.36" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.01</text>
#>       <polyline points="287.75,326.16 370.96,326.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="332.02" cy="326.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.55 (-0.949, 3.75)</text>
#>       <text x="69.36" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="274.35,340.56 304.59,340.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.52" cy="340.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="122.70px" lengthAdjust="spacingAndGlyphs">-0.849 (-1.71, 0.00161)</text>
#>       <text x="69.36" y="362.69" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.64px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Sibutramine</text>
#>       <text x="69.36" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="273.64,376.56 314.66,376.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="292.54" cy="376.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.678 (-1.75, 0.570)</text>
#>       <text x="69.36" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.02</text>
#>       <polyline points="308.71,390.96 374.26,390.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="337.15" cy="390.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="94.69px" lengthAdjust="spacingAndGlyphs">1.84 (0.234, 3.93)</text>
#>       <text x="69.36" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="284.67,405.36 313.93,405.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="299.85" cy="405.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.266 (-1.12, 0.528)</text>
#>       <text x="69.36" y="427.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="124.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Placebo</text>
#>       <text x="69.36" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="258.98,441.36 317.46,441.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.00" cy="441.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.991 (-2.57, 0.728)</text>
#>       <text x="69.36" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.095</text>
#>       <polyline points="240.23,455.76 277.57,455.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="258.58" cy="455.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.60 (-3.63, -1.52)</text>
#>       <text x="69.36" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="253.06,470.16 287.17,470.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="267.92" cy="470.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-2.07 (-2.91, -0.982)</text>
#>       <text x="69.36" y="492.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="120.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Orlistat</text>
#>       <text x="69.36" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="262.40,506.16 294.16,506.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="278.56" cy="506.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.47 (-2.38, -0.587)</text>
#>       <text x="69.36" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="16.67px" lengthAdjust="spacingAndGlyphs">0.1</text>
#>       <polyline points="277.63,520.56 329.68,520.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="303.30" cy="520.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.0712 (-1.52, 1.42)</text>
#>       <text x="69.36" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="275.04,534.96 305.21,534.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.47" cy="534.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.965 (-1.67, 0.0363)</text>
#>       <text x="69.36" y="557.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="147.33px" lengthAdjust="spacingAndGlyphs">Metformin vs Sibutramine</text>
#>       <text x="69.36" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="288.38,570.96 345.60,570.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="314.20" cy="570.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="105.36px" lengthAdjust="spacingAndGlyphs">0.544 (-0.913, 2.32)</text>
#>       <text x="69.36" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="151.53" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.07</text>
#>       <polyline points="266.49,585.36 306.60,585.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.89" cy="585.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="108.47px" lengthAdjust="spacingAndGlyphs">-0.998 (-2.15, 0.115)</text>
#>       <text x="69.36" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="283.42,599.76 316.75,599.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="297.50" cy="599.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="383.93" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.399 (-1.19, 0.688)</text>
#>       <polyline points="304.56,614.16 304.56,88.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="233.70,606.96 375.43,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="233.70,614.16 233.70,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="375.43,614.16 375.43,606.96 " style="stroke-width: 0.75;"/>
#>       <text x="304.56" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="233.70" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="10.67px" lengthAdjust="spacingAndGlyphs">-4</text>
#>       <text x="375.43" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <text x="302.40" y="25.83" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="256.73px" lengthAdjust="spacingAndGlyphs">Inconsistency test with nodesplitting </text>
#>       <text x="302.40" y="43.11" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="139.97px" lengthAdjust="spacingAndGlyphs">model for all studies</text>
#>     </g>
#>   </g>
#> </svg>
#> 
# }
```
