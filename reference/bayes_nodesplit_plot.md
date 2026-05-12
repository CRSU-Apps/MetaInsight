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
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="58 6 468 630">
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
#>       <text x="67.36" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.67px" lengthAdjust="spacingAndGlyphs">Study</text>
#>       <text x="149.53" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="40.69px" lengthAdjust="spacingAndGlyphs">P-value</text>
#>       <text x="445.29" y="85.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="142.48px" lengthAdjust="spacingAndGlyphs">Mean Difference (95% CrI)</text>
#>       <text x="67.36" y="103.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="135.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Placebo</text>
#>       <text x="67.36" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="266.86,117.36 280.07,117.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="272.61" cy="117.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.69 (-2.02, -1.27)</text>
#>       <text x="67.36" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.28</text>
#>       <polyline points="261.05,131.76 314.94,131.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="290.78" cy="131.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.665 (-2.34, 0.699)</text>
#>       <text x="67.36" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="268.49,146.16 279.20,146.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="273.66" cy="146.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.63 (-1.92, -1.32)</text>
#>       <text x="67.36" y="168.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="108.00px" lengthAdjust="spacingAndGlyphs">Orlistat vs Placebo</text>
#>       <text x="67.36" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="272.97,182.16 291.62,182.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="281.50" cy="182.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.19 (-1.67, -0.618)</text>
#>       <text x="67.36" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="16.67px" lengthAdjust="spacingAndGlyphs">0.9</text>
#>       <polyline points="265.96,196.56 306.55,196.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="282.76" cy="196.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="102.69px" lengthAdjust="spacingAndGlyphs">-1.12 (-2.07, 0.225)</text>
#>       <text x="67.36" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="276.84,210.96 291.43,210.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="283.67" cy="210.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.07 (-1.45, -0.628)</text>
#>       <text x="67.36" y="233.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="131.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Orlistat</text>
#>       <text x="67.36" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="282.35,246.96 306.62,246.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="294.65" cy="246.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.446 (-1.14, 0.229)</text>
#>       <text x="67.36" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.515</text>
#>       <polyline points="274.38,261.36 302.46,261.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="288.23" cy="261.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="126.70px" lengthAdjust="spacingAndGlyphs">-0.809 (-1.59, -0.00598)</text>
#>       <text x="67.36" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="283.68,275.76 300.12,275.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="292.57" cy="275.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="113.36px" lengthAdjust="spacingAndGlyphs">-0.564 (-1.07, -0.138)</text>
#>       <text x="67.36" y="297.89" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="119.31px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Orlistat</text>
#>       <text x="67.36" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="257.55,311.76 295.41,311.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="275.55" cy="311.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.52 (-2.54, -0.404)</text>
#>       <text x="67.36" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.015</text>
#>       <polyline points="287.89,326.16 367.64,326.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="325.39" cy="326.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.29 (-0.829, 3.67)</text>
#>       <text x="67.36" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="271.57,340.56 301.74,340.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.34" cy="340.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="120.03px" lengthAdjust="spacingAndGlyphs">-0.916 (-1.75, -0.0465)</text>
#>       <text x="67.36" y="362.69" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.64px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Sibutramine</text>
#>       <text x="67.36" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="268.25,376.56 308.09,376.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.12" cy="376.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.759 (-1.94, 0.312)</text>
#>       <text x="67.36" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.04</text>
#>       <polyline points="299.63,390.96 364.85,390.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="331.66" cy="390.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.64 (-0.166, 3.52)</text>
#>       <text x="67.36" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="280.92,405.36 311.84,405.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="296.68" cy="405.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.332 (-1.22, 0.524)</text>
#>       <text x="67.36" y="427.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="124.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Placebo</text>
#>       <text x="67.36" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="250.18,441.36 310.95,441.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="282.75" cy="441.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="102.69px" lengthAdjust="spacingAndGlyphs">-1.12 (-2.96, 0.474)</text>
#>       <text x="67.36" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.165</text>
#>       <polyline points="235.89,455.76 278.69,455.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="256.76" cy="455.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.59 (-3.76, -1.35)</text>
#>       <text x="67.36" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="250.98,470.16 283.53,470.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="266.01" cy="470.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.06 (-2.91, -1.07)</text>
#>       <text x="67.36" y="492.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="120.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Orlistat</text>
#>       <text x="67.36" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="259.00,506.16 299.61,506.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="279.70" cy="506.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.29 (-2.46, -0.167)</text>
#>       <text x="67.36" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.055</text>
#>       <polyline points="286.95,520.56 333.36,520.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="310.54" cy="520.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="105.36px" lengthAdjust="spacingAndGlyphs">0.450 (-0.881, 1.74)</text>
#>       <text x="67.36" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="269.18,534.96 302.94,534.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="285.14" cy="534.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.983 (-1.88, 0.0210)</text>
#>       <text x="67.36" y="557.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="147.33px" lengthAdjust="spacingAndGlyphs">Metformin vs Sibutramine</text>
#>       <text x="67.36" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="277.00,570.96 340.18,570.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="311.96" cy="570.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">0.530 (-1.44, 2.12)</text>
#>       <text x="67.36" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="149.53" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.185</text>
#>       <polyline points="263.44,585.36 306.86,585.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="284.97" cy="585.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.993 (-2.21, 0.242)</text>
#>       <text x="67.36" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="280.79,599.76 312.47,599.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="295.43" cy="599.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="381.93" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.403 (-1.23, 0.559)</text>
#>       <polyline points="302.56,614.16 302.56,88.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="231.70,606.96 373.43,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="231.70,614.16 231.70,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="373.43,614.16 373.43,606.96 " style="stroke-width: 0.75;"/>
#>       <text x="302.56" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="231.70" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="10.67px" lengthAdjust="spacingAndGlyphs">-4</text>
#>       <text x="373.43" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <text x="302.40" y="25.83" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="256.73px" lengthAdjust="spacingAndGlyphs">Inconsistency test with nodesplitting </text>
#>       <text x="302.40" y="43.11" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="139.97px" lengthAdjust="spacingAndGlyphs">model for all studies</text>
#>     </g>
#>   </g>
#> </svg>
#> 
# }
```
