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
#>       <polyline points="270.99,117.36 282.14,117.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="276.49" cy="117.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="121.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.66 (-1.97, -1.34)</text>
#>       <text x="70.70" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.195</text>
#>       <polyline points="267.21,131.76 320.51,131.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="294.96" cy="131.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="135.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.617 (-2.18, 0.824)</text>
#>       <text x="70.70" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="271.65,146.16 281.76,146.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="276.70" cy="146.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="150.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-1.65 (-1.93, -1.36)</text>
#>       <text x="70.70" y="168.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="108.00px" lengthAdjust="spacingAndGlyphs">Orlistat vs Placebo</text>
#>       <text x="70.70" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="276.51,182.16 296.39,182.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.15" cy="182.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="186.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="105.80px" lengthAdjust="spacingAndGlyphs">-1.11 (-1.66, -0.537)</text>
#>       <text x="70.70" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.73</text>
#>       <polyline points="269.34,196.56 310.05,196.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="290.19" cy="196.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="200.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.887 (-2.06, 0.234)</text>
#>       <text x="70.70" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="278.93,210.96 294.72,210.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="286.13" cy="210.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="215.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.12 (-1.52, -0.631)</text>
#>       <text x="70.70" y="233.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="131.33px" lengthAdjust="spacingAndGlyphs">Sibutramine vs Orlistat</text>
#>       <text x="70.70" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="287.66,246.96 310.48,246.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="298.69" cy="246.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="251.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.407 (-1.03, 0.259)</text>
#>       <text x="70.70" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.61</text>
#>       <polyline points="276.70,261.36 305.39,261.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="293.73" cy="261.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="265.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="120.03px" lengthAdjust="spacingAndGlyphs">-0.687 (-1.65, -0.0287)</text>
#>       <text x="70.70" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="289.05,275.76 303.83,275.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="296.33" cy="275.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="279.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="119.14px" lengthAdjust="spacingAndGlyphs">-0.540 (-0.951, -0.117)</text>
#>       <text x="70.70" y="297.89" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="119.31px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Orlistat</text>
#>       <text x="70.70" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="259.09,311.76 296.00,311.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="279.41" cy="311.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="315.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.50 (-2.64, -0.559)</text>
#>       <text x="70.70" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="10.00px" lengthAdjust="spacingAndGlyphs"> 0</text>
#>       <polyline points="301.28,326.16 363.80,326.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="333.13" cy="326.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="330.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.54 (-0.261, 3.27)</text>
#>       <text x="70.70" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="272.81,340.56 306.74,340.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.29" cy="340.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="344.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-1.05 (-1.87, 0.0472)</text>
#>       <text x="70.70" y="362.69" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="146.64px" lengthAdjust="spacingAndGlyphs">Orli_Sibut vs Sibutramine</text>
#>       <text x="70.70" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="272.56,376.56 313.49,376.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="295.28" cy="376.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="380.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.599 (-1.88, 0.428)</text>
#>       <text x="70.70" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.06</text>
#>       <polyline points="297.20,390.96 364.55,390.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="334.38" cy="390.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="395.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">1.61 (-0.491, 3.31)</text>
#>       <text x="70.70" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="281.77,405.36 317.53,405.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="296.65" cy="405.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="409.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.522 (-1.36, 0.657)</text>
#>       <text x="70.70" y="427.49" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="124.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Placebo</text>
#>       <text x="70.70" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="258.97,441.36 313.52,441.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="287.26" cy="441.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="445.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="102.69px" lengthAdjust="spacingAndGlyphs">-1.05 (-2.65, 0.430)</text>
#>       <text x="70.70" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="30.02px" lengthAdjust="spacingAndGlyphs">0.155</text>
#>       <polyline points="241.38,455.76 280.15,455.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="261.34" cy="455.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="459.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="100.02px" lengthAdjust="spacingAndGlyphs">-2.52 (-3.64, -1.45)</text>
#>       <text x="70.70" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="254.00,470.16 288.24,470.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="269.20" cy="470.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="474.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-2.07 (-2.93, -0.997)</text>
#>       <text x="70.70" y="492.29" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="120.00px" lengthAdjust="spacingAndGlyphs">Metformin vs Orlistat</text>
#>       <text x="70.70" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="257.54,506.16 298.48,506.16 " style="stroke-width: 0.75;"/>
#>       <circle cx="280.58" cy="506.16" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="510.29" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="106.69px" lengthAdjust="spacingAndGlyphs">-1.43 (-2.73, -0.419)</text>
#>       <text x="70.70" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.12</text>
#>       <polyline points="278.78,520.56 339.45,520.56 " style="stroke-width: 0.75;"/>
#>       <circle cx="308.79" cy="520.56" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="524.69" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">0.163 (-1.53, 1.89)</text>
#>       <text x="70.70" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="275.60,534.96 306.50,534.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="288.60" cy="534.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="539.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="116.03px" lengthAdjust="spacingAndGlyphs">-0.977 (-1.71, 0.0338)</text>
#>       <text x="70.70" y="557.09" style="font-size: 12.00px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="147.33px" lengthAdjust="spacingAndGlyphs">Metformin vs Sibutramine</text>
#>       <text x="70.70" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="29.34px" lengthAdjust="spacingAndGlyphs">direct</text>
#>       <polyline points="286.96,570.96 346.48,570.96 " style="stroke-width: 0.75;"/>
#>       <circle cx="317.44" cy="570.96" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="575.09" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="98.69px" lengthAdjust="spacingAndGlyphs">0.652 (-1.07, 2.29)</text>
#>       <text x="70.70" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="38.69px" lengthAdjust="spacingAndGlyphs">indirect</text>
#>       <text x="152.87" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="23.34px" lengthAdjust="spacingAndGlyphs">0.09</text>
#>       <polyline points="268.62,585.36 310.09,585.36 " style="stroke-width: 0.75;"/>
#>       <circle cx="289.25" cy="585.36" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="589.49" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.940 (-2.10, 0.236)</text>
#>       <text x="70.70" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="42.02px" lengthAdjust="spacingAndGlyphs">network</text>
#>       <polyline points="283.51,599.76 318.04,599.76 " style="stroke-width: 0.75;"/>
#>       <circle cx="298.33" cy="599.76" r="2.88" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="385.27" y="603.89" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="109.36px" lengthAdjust="spacingAndGlyphs">-0.427 (-1.26, 0.685)</text>
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
