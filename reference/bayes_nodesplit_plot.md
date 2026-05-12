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
#>       <text x="71.14" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.67px" lengthAdjust="spacingAndGlyphs">Study</text>
#>       <text x="153.31" y="85.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="40.69px" lengthAdjust="spacingAndGlyphs">P-value</text>
#>       <text x="445.29" y="85.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="142.48px" lengthAdjust="spacingAndGlyphs">Mean Difference (95% CrI)</text>
#>       <text x="71.14" y="103.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="135.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Placebo</text>
#>       <text x="71.14" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="270.78,117.36 282.64,117.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="276.34" cy="117.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.69 (-2.01, -1.34)</text>
#>       <text x="71.14" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.195</text>
#>       <polyline points="270.73,131.76 316.18,131.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="291.96" cy="131.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.812 (-2.01, 0.555)</text>
#>       <text x="71.14" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="272.76,146.16 283.46,146.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="277.59" cy="146.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.62 (-1.90, -1.29)</text>
#>       <text x="71.14" y="168.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="108.00px" lengthAdjust="spacingAndGlyphs">Orlistat vs Placebo</text>
#>       <text x="71.14" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="278.36,182.16 293.37,182.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.21" cy="182.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.14 (-1.58, -0.733)</text>
#>       <text x="71.14" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.945</text>
#>       <polyline points="268.26,196.56 306.15,196.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.14" cy="196.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="113.36px" lengthAdjust="spacingAndGlyphs">-1.08 (-2.15, -0.0109)</text>
#>       <text x="71.14" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="281.64,210.96 294.98,210.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.24" cy="210.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.08 (-1.39, -0.642)</text>
#>       <text x="71.14" y="233.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="131.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Orlistat</text>
#>       <text x="71.14" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="287.29,246.96 307.92,246.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="297.30" cy="246.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.510 (-1.08, 0.0887)</text>
#>       <text x="71.14" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.55</text>
#>       <polyline points="277.74,261.36 304.02,261.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="292.87" cy="261.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="113.36px" lengthAdjust="spacingAndGlyphs">-0.760 (-1.61, -0.131)</text>
#>       <text x="71.14" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="288.79,275.76 304.36,275.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="296.26" cy="275.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="119.14px" lengthAdjust="spacingAndGlyphs">-0.569 (-0.991, -0.112)</text>
#>       <text x="71.14" y="297.89" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="119.31px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Orlistat</text>
#>       <text x="71.14" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="263.87,311.76 299.22,311.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="279.64" cy="311.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.51 (-2.40, -0.402)</text>
#>       <text x="71.14" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.015</text>
#>       <polyline points="293.20,326.16 361.54,326.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="334.15" cy="326.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.57 (-0.742, 3.12)</text>
#>       <text x="71.14" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="272.53,340.56 308.04,340.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.22" cy="340.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.966 (-1.91, 0.0959)</text>
#>       <text x="71.14" y="362.69" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.64px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Sibutramine</text>
#>       <text x="71.14" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="276.02,376.56 310.71,376.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="293.57" cy="376.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.721 (-1.71, 0.246)</text>
#>       <text x="71.14" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.085</text>
#>       <polyline points="293.91,390.96 372.43,390.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="329.48" cy="390.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.31 (-0.702, 3.73)</text>
#>       <text x="71.14" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="282.66,405.36 315.97,405.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="299.01" cy="405.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.414 (-1.34, 0.543)</text>
#>       <text x="71.14" y="427.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="124.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Placebo</text>
#>       <text x="71.14" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="256.46,441.36 316.61,441.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.48" cy="441.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="102.69px" lengthAdjust="spacingAndGlyphs">-1.07 (-2.82, 0.579)</text>
#>       <text x="71.14" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.16</text>
#>       <polyline points="241.00,455.76 283.57,455.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="260.62" cy="455.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.58 (-3.69, -1.29)</text>
#>       <text x="71.14" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="254.12,470.16 287.59,470.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="269.46" cy="470.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.08 (-2.95, -1.06)</text>
#>       <text x="71.14" y="492.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="120.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Orlistat</text>
#>       <text x="71.14" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="263.63,506.16 302.43,506.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="282.02" cy="506.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.37 (-2.41, -0.221)</text>
#>       <text x="71.14" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.09</text>
#>       <polyline points="287.85,520.56 330.62,520.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="310.41" cy="520.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">0.229 (-1.04, 1.37)</text>
#>       <text x="71.14" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="273.42,534.96 304.54,534.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="288.18" cy="534.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.03 (-1.86, -0.102)</text>
#>       <text x="71.14" y="557.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="147.33px" lengthAdjust="spacingAndGlyphs">Metformin vs Sibutramine</text>
#>       <text x="71.14" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="293.86,570.96 347.49,570.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="318.51" cy="570.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="105.36px" lengthAdjust="spacingAndGlyphs">0.687 (-0.705, 2.32)</text>
#>       <text x="71.14" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="153.31" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.12px" lengthAdjust="spacingAndGlyphs">0.115</text>
#>       <polyline points="271.21,585.36 313.10,585.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="290.60" cy="585.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.889 (-1.98, 0.381)</text>
#>       <text x="71.14" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="282.55,599.76 316.11,599.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="298.60" cy="599.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.72" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.437 (-1.34, 0.551)</text>
#>       <polyline points="306.35,614.16 306.35,88.56 " style="stroke-width: 0.75;"/>
#>       <polyline points="235.48,606.96 377.21,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="235.48,614.16 235.48,606.96 " style="stroke-width: 0.75;"/>
#>       <polyline points="377.21,614.16 377.21,606.96 " style="stroke-width: 0.75;"/>
#>       <text x="306.35" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="235.48" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="10.67px" lengthAdjust="spacingAndGlyphs">-4</text>
#>       <text x="377.21" y="625.49" text-anchor="middle" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <text x="302.40" y="25.83" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="256.73px" lengthAdjust="spacingAndGlyphs">Inconsistency test with nodesplitting </text>
#>       <text x="302.40" y="43.11" text-anchor="middle" style="font-size: 14.40px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="139.97px" lengthAdjust="spacingAndGlyphs">model for all studies</text>
#>     </g>
#>   </g>
#> </svg>
#> 
# }
```
