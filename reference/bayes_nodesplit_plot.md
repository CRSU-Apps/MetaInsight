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
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="62 6 464 630">
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
#>       <text x="70.70" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.67px" lengthAdjust="spacingAndGlyphs">Study</text>
#>       <text x="152.87" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="40.69px" lengthAdjust="spacingAndGlyphs">P-value</text>
#>       <text x="445.29" y="85.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="142.48px" lengthAdjust="spacingAndGlyphs">Mean Difference (95% CrI)</text>
#>       <text x="70.70" y="103.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="135.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Placebo</text>
#>       <text x="70.70" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="269.37,117.36 281.46,117.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="275.50" cy="117.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.72 (-2.06, -1.38)</text>
#>       <text x="70.70" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="16.67px" lengthAdjust="spacingAndGlyphs">0.2</text>
#>       <polyline points="267.79,131.76 309.74,131.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.41" cy="131.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.931 (-2.15, 0.217)</text>
#>       <text x="70.70" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="271.00,146.16 283.10,146.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="277.14" cy="146.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.62 (-1.97, -1.29)</text>
#>       <text x="70.70" y="168.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="108.00px" lengthAdjust="spacingAndGlyphs">Orlistat vs Placebo</text>
#>       <text x="70.70" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="277.80,182.16 293.66,182.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.00" cy="182.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.12 (-1.59, -0.691)</text>
#>       <text x="70.70" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.935</text>
#>       <polyline points="263.24,196.56 302.46,196.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="284.91" cy="196.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.18 (-2.41, -0.194)</text>
#>       <text x="70.70" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="280.60,210.96 293.76,210.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.61" cy="210.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.09 (-1.43, -0.685)</text>
#>       <text x="70.70" y="233.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="131.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Orlistat</text>
#>       <text x="70.70" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="286.60,246.96 310.77,246.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="297.74" cy="246.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.461 (-1.09, 0.275)</text>
#>       <text x="70.70" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.65</text>
#>       <polyline points="280.10,261.36 307.30,261.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="294.43" cy="261.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.648 (-1.46, 0.0789)</text>
#>       <text x="70.70" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="289.72,275.76 302.72,275.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="296.07" cy="275.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="120.03px" lengthAdjust="spacingAndGlyphs">-0.555 (-0.914, -0.180)</text>
#>       <text x="70.70" y="297.89" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="119.31px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Orlistat</text>
#>       <text x="70.70" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="260.61,311.76 296.21,311.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="279.16" cy="311.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.51 (-2.56, -0.547)</text>
#>       <text x="70.70" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.02</text>
#>       <polyline points="295.87,326.16 363.55,326.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="329.37" cy="326.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.32 (-0.566, 3.25)</text>
#>       <text x="70.70" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="275.23,340.56 304.25,340.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.46" cy="340.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="120.03px" lengthAdjust="spacingAndGlyphs">-0.928 (-1.73, -0.0930)</text>
#>       <text x="70.70" y="362.69" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.64px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Sibutramine</text>
#>       <text x="70.70" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="273.53,376.56 315.06,376.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="292.25" cy="376.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.771 (-1.83, 0.517)</text>
#>       <text x="70.70" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.055</text>
#>       <polyline points="305.88,390.96 374.11,390.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="335.48" cy="390.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="118.70px" lengthAdjust="spacingAndGlyphs">1.67 (-0.000915, 3.85)</text>
#>       <text x="70.70" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="284.20,405.36 313.76,405.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="299.29" cy="405.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.373 (-1.23, 0.444)</text>
#>       <text x="70.70" y="427.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="124.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Placebo</text>
#>       <text x="70.70" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="259.10,441.36 317.73,441.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.63" cy="441.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="102.69px" lengthAdjust="spacingAndGlyphs">-1.03 (-2.64, 0.668)</text>
#>       <text x="70.70" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.095</text>
#>       <polyline points="238.72,455.76 285.13,455.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="260.54" cy="455.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.56 (-3.79, -1.17)</text>
#>       <text x="70.70" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="253.31,470.16 284.89,470.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="268.85" cy="470.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.09 (-2.97, -1.19)</text>
#>       <text x="70.70" y="492.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="120.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Orlistat</text>
#>       <text x="70.70" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="260.66,506.16 299.20,506.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="279.47" cy="506.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.49 (-2.55, -0.378)</text>
#>       <text x="70.70" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">0.115</text>
#>       <polyline points="281.95,520.56 336.14,520.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="305.75" cy="520.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.00849 (-1.35, 1.71)</text>
#>       <text x="70.70" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="274.15,534.96 303.25,534.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="288.11" cy="534.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.00 (-1.79, -0.149)</text>
#>       <text x="70.70" y="557.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="147.33px" lengthAdjust="spacingAndGlyphs">Metformin vs Sibutramine</text>
#>       <text x="70.70" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="294.97,570.96 347.47,570.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="321.47" cy="570.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="105.36px" lengthAdjust="spacingAndGlyphs">0.879 (-0.617, 2.35)</text>
#>       <text x="70.70" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.085</text>
#>       <polyline points="269.64,585.36 310.42,585.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="288.33" cy="585.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.992 (-2.05, 0.255)</text>
#>       <text x="70.70" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="283.20,599.76 312.16,599.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="297.49" cy="599.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.475 (-1.28, 0.353)</text>
#>       <polyline points="305.90,614.16 305.90,88.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="235.03,606.96 376.77,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="235.03,614.16 235.03,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="376.77,614.16 376.77,606.96 " style="stroke-width: 0.75;"/>
#>       <text x="305.90" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="235.03" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="10.67px" lengthAdjust="spacingAndGlyphs">-4</text>
#>       <text x="376.77" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <text x="302.40" y="25.83" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="256.73px" lengthAdjust="spacingAndGlyphs">Inconsistency test with nodesplitting </text>
#>       <text x="302.40" y="43.11" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="139.97px" lengthAdjust="spacingAndGlyphs">model for all studies</text>
#>     </g>
#>   </g>
#> </svg>
#> 
# }
```
