# Summary forest plot matrix

Produce a summary forest plot matrix with treatments ranked by SUCRA
score, determined by
[`netmeta::rankogram()`](https://rdrr.io/pkg/netmeta/man/rankogram.html).
This function can only be used when `configured_data` contains between 3
and 10 treatments.

## Usage

``` r
freq_summary(configured_data, plot_title = "", logger = NULL)
```

## Arguments

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- plot_title:

  character. Title of the plot. Default is no title.

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

freq_summary(configured_data = configured_data)
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="-8 11 736 718">
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
#>     <rect width="720.00" height="720.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=">
#>         <rect x="0.00" y="0.00" width="720.00" height="720.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxODAuMDB8MTkuMDF8MTgyLjM4">
#>         <rect x="0.00" y="19.01" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxODAuMDB8MTkuMDF8MTgyLjM4)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMi4zOHwxNzcuNjJ8MjAuOTF8MTc5LjUy">
#>         <rect x="2.38" y="20.91" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8MjAuOTF8MTc5LjUy)">
#>       <rect x="2.38" y="20.91" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <rect x="2.38" y="20.91" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="90.00" y="55.82" text-anchor="middle" style="font-size: 13.24px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="61.78px" lengthAdjust="spacingAndGlyphs">the_Great</text>
#>       <text x="94.87" y="106.92" text-anchor="middle" style="font-size: 11.92px; font-family: &quot;Liberation Sans&quot;;" textLength="48.00px" lengthAdjust="spacingAndGlyphs">Rank = 1</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="2.38,179.52 177.62,179.52 177.62,20.91 2.38,20.91 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMi4zOHwxNzcuNjJ8MTg0LjI4fDM0Mi44OQ==">
#>         <rect x="2.38" y="184.28" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8MTg0LjI4fDM0Mi44OQ==)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxODAuMDB8MTgyLjM4fDM0NS43NA==">
#>         <rect x="0.00" y="182.38" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxODAuMDB8MTgyLjM4fDM0NS43NA==)">
#> </g>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8MTg0LjI4fDM0Mi44OQ==)">
#>       <rect x="2.38" y="184.28" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <polyline points="138.68,337.02 138.68,190.15 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="23.10,239.11 65.13,239.11 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="23.10,244.00 23.10,234.21 " style="stroke-width: 1.12;"/>
#>       <polyline points="65.13,244.00 65.13,234.21 " style="stroke-width: 1.12;"/>
#>       <polyline points="38.13,239.11 50.10,239.11 " style="stroke-width: 1.12;"/>
#>       <polyline points="38.13,248.90 38.13,229.32 " style="stroke-width: 1.12;"/>
#>       <polyline points="50.10,248.90 50.10,229.32 " style="stroke-width: 1.12;"/>
#>       <polygon points="41.97,241.25 46.25,241.25 46.25,236.97 41.97,236.97 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="38.13,288.06 50.10,288.06 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="38.13,297.85 38.13,278.27 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="50.10,297.85 50.10,278.27 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polygon points="41.97,290.20 46.25,290.20 46.25,285.92 41.97,285.92 " style="stroke-width: 0.75; stroke: none; fill: #8C8C8C;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="2.38,342.89 177.62,342.89 177.62,184.28 2.38,184.28 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMi4zOHwxNzcuNjJ8MzQ3LjY0fDUwNi4yNg==">
#>         <rect x="2.38" y="347.64" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8MzQ3LjY0fDUwNi4yNg==)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxODAuMDB8MzQ1Ljc0fDUwOS4xMQ==">
#>         <rect x="0.00" y="345.74" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxODAuMDB8MzQ1Ljc0fDUwOS4xMQ==)">
#> </g>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8MzQ3LjY0fDUwNi4yNg==)">
#>       <rect x="2.38" y="347.64" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <polyline points="138.68,500.39 138.68,353.52 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="15.24,402.48 64.15,402.48 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="15.24,407.37 15.24,397.58 " style="stroke-width: 1.12;"/>
#>       <polyline points="64.15,407.37 64.15,397.58 " style="stroke-width: 1.12;"/>
#>       <polyline points="31.43,402.48 47.96,402.48 " style="stroke-width: 1.12;"/>
#>       <polyline points="31.43,412.27 31.43,392.68 " style="stroke-width: 1.12;"/>
#>       <polyline points="47.96,412.27 47.96,392.68 " style="stroke-width: 1.12;"/>
#>       <polygon points="37.56,404.61 41.84,404.61 41.84,400.34 37.56,400.34 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="31.43,451.43 47.96,451.43 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="31.43,461.22 31.43,441.64 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="47.96,461.22 47.96,441.64 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polygon points="37.56,453.57 41.84,453.57 41.84,449.29 37.56,449.29 " style="stroke-width: 0.75; stroke: none; fill: #8C8C8C;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="2.38,506.26 177.62,506.26 177.62,347.64 2.38,347.64 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMi4zOHwxNzcuNjJ8NTExLjAxfDY2OS42Mw==">
#>         <rect x="2.38" y="511.01" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8NTExLjAxfDY2OS42Mw==)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxODAuMDB8NTA5LjExfDY3Mi40OA==">
#>         <rect x="0.00" y="509.11" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxODAuMDB8NTA5LjExfDY3Mi40OA==)">
#> </g>
#>     <g clip-path="url(#cpMi4zOHwxNzcuNjJ8NTExLjAxfDY2OS42Mw==)">
#>       <rect x="2.38" y="511.01" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <polyline points="138.68,663.75 138.68,516.89 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="18.82,565.84 60.58,565.84 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="18.82,570.74 18.82,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="60.58,570.74 60.58,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="33.81,565.84 45.58,565.84 " style="stroke-width: 1.12;"/>
#>       <polyline points="33.81,575.63 33.81,556.05 " style="stroke-width: 1.12;"/>
#>       <polyline points="45.58,575.63 45.58,556.05 " style="stroke-width: 1.12;"/>
#>       <polygon points="37.56,567.98 41.84,567.98 41.84,563.70 37.56,563.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>       <polyline points="33.81,614.80 45.58,614.80 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="33.81,624.59 33.81,605.01 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polyline points="45.58,624.59 45.58,605.01 " style="stroke-width: 1.12; stroke: #8C8C8C;"/>
#>       <polygon points="37.56,616.94 41.84,616.94 41.84,612.66 37.56,612.66 " style="stroke-width: 0.75; stroke: none; fill: #8C8C8C;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="16.98" y1="669.63" x2="138.68" y2="669.63" style="stroke-width: 0.75;"/>
#>       <line x1="16.98" y1="669.63" x2="16.98" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="57.55" y1="669.63" x2="57.55" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="98.11" y1="669.63" x2="98.11" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="138.68" y1="669.63" x2="138.68" y2="677.56" style="stroke-width: 0.75;"/>
#>       <text x="16.98" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-15</text>
#>       <text x="57.55" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-10</text>
#>       <text x="98.11" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="4.22px" lengthAdjust="spacingAndGlyphs">-5</text>
#>       <text x="138.68" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="2.64px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <polygon points="2.38,669.63 177.62,669.63 177.62,511.01 2.38,511.01 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMTgyLjM4fDM1Ny42MnwyMC45MXwxNzkuNTI=">
#>         <rect x="182.38" y="20.91" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwyMC45MXwxNzkuNTI=)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTgwLjAwfDM2MC4wMHwxOS4wMXwxODIuMzg=">
#>         <rect x="180.00" y="19.01" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgwLjAwfDM2MC4wMHwxOS4wMXwxODIuMzg=)">
#> </g>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwyMC45MXwxNzkuNTI=)">
#>       <rect x="182.38" y="20.91" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="270.00" y="56.09" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="38.70px" lengthAdjust="spacingAndGlyphs">-11.66</text>
#>       <text x="270.00" y="79.37" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="108.31px" lengthAdjust="spacingAndGlyphs">(-12.39 to -10.92)</text>
#>       <text x="270.00" y="129.53" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="38.70px" lengthAdjust="spacingAndGlyphs">-11.66</text>
#>       <text x="270.00" y="152.80" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="108.31px" lengthAdjust="spacingAndGlyphs">(-12.39 to -10.92)</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="182.38,179.52 357.62,179.52 357.62,20.91 182.38,20.91 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMTgyLjM4fDM1Ny42MnwxODQuMjh8MzQyLjg5">
#>         <rect x="182.38" y="184.28" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwxODQuMjh8MzQyLjg5)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTgwLjAwfDM2MC4wMHwxODIuMzh8MzQ1Ljc0">
#>         <rect x="180.00" y="182.38" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgwLjAwfDM2MC4wMHwxODIuMzh8MzQ1Ljc0)">
#> </g>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwxODQuMjh8MzQyLjg5)">
#>       <rect x="182.38" y="184.28" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <rect x="182.38" y="184.28" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="270.00" y="219.18" text-anchor="middle" style="font-size: 13.24px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="79.88px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <text x="274.87" y="270.29" text-anchor="middle" style="font-size: 11.92px; font-family: &quot;Liberation Sans&quot;;" textLength="48.00px" lengthAdjust="spacingAndGlyphs">Rank = 2</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="182.38,342.89 357.62,342.89 357.62,184.28 182.38,184.28 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMTgyLjM4fDM1Ny42MnwzNDcuNjR8NTA2LjI2">
#>         <rect x="182.38" y="347.64" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwzNDcuNjR8NTA2LjI2)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTgwLjAwfDM2MC4wMHwzNDUuNzR8NTA5LjEx">
#>         <rect x="180.00" y="345.74" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgwLjAwfDM2MC4wMHwzNDUuNzR8NTA5LjEx)">
#> </g>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42MnwzNDcuNjR8NTA2LjI2)">
#>       <rect x="182.38" y="347.64" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <polyline points="318.68,500.39 318.68,353.52 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="286.50,402.48 342.02,402.48 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="286.50,407.37 286.50,397.58 " style="stroke-width: 1.12;"/>
#>       <polyline points="342.02,407.37 342.02,397.58 " style="stroke-width: 1.12;"/>
#>       <polyline points="304.06,402.48 324.47,402.48 " style="stroke-width: 1.12;"/>
#>       <polyline points="304.06,412.27 304.06,392.68 " style="stroke-width: 1.12;"/>
#>       <polyline points="324.47,412.27 324.47,392.68 " style="stroke-width: 1.12;"/>
#>       <polygon points="312.13,404.61 316.40,404.61 316.40,400.34 312.13,400.34 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="182.38,506.26 357.62,506.26 357.62,347.64 182.38,347.64 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMTgyLjM4fDM1Ny42Mnw1MTEuMDF8NjY5LjYz">
#>         <rect x="182.38" y="511.01" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42Mnw1MTEuMDF8NjY5LjYz)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTgwLjAwfDM2MC4wMHw1MDkuMTF8NjcyLjQ4">
#>         <rect x="180.00" y="509.11" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgwLjAwfDM2MC4wMHw1MDkuMTF8NjcyLjQ4)">
#> </g>
#>     <g clip-path="url(#cpMTgyLjM4fDM1Ny42Mnw1MTEuMDF8NjY5LjYz)">
#>       <rect x="182.38" y="511.01" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <polyline points="318.68,663.75 318.68,516.89 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="289.60,565.84 338.93,565.84 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="289.60,570.74 289.60,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="338.93,570.74 338.93,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="305.87,565.84 322.66,565.84 " style="stroke-width: 1.12;"/>
#>       <polyline points="305.87,575.63 305.87,556.05 " style="stroke-width: 1.12;"/>
#>       <polyline points="322.66,575.63 322.66,556.05 " style="stroke-width: 1.12;"/>
#>       <polygon points="312.13,567.98 316.40,567.98 316.40,563.70 312.13,563.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="196.98" y1="669.63" x2="318.68" y2="669.63" style="stroke-width: 0.75;"/>
#>       <line x1="196.98" y1="669.63" x2="196.98" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="237.55" y1="669.63" x2="237.55" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="278.11" y1="669.63" x2="278.11" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="318.68" y1="669.63" x2="318.68" y2="677.56" style="stroke-width: 0.75;"/>
#>       <text x="196.98" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-15</text>
#>       <text x="237.55" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-10</text>
#>       <text x="278.11" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="4.22px" lengthAdjust="spacingAndGlyphs">-5</text>
#>       <text x="318.68" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="2.64px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <polygon points="182.38,669.63 357.62,669.63 357.62,511.01 182.38,511.01 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzYyLjM4fDUzNy42MnwyMC45MXwxNzkuNTI=">
#>         <rect x="362.38" y="20.91" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwyMC45MXwxNzkuNTI=)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMzYwLjAwfDU0MC4wMHwxOS4wMXwxODIuMzg=">
#>         <rect x="360.00" y="19.01" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYwLjAwfDU0MC4wMHwxOS4wMXwxODIuMzg=)">
#> </g>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwyMC45MXwxNzkuNTI=)">
#>       <rect x="362.38" y="20.91" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <text x="450.00" y="56.09" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="39.75px" lengthAdjust="spacingAndGlyphs">-12.20</text>
#>       <text x="450.00" y="79.37" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="107.27px" lengthAdjust="spacingAndGlyphs">(-13.22 to -11.18)</text>
#>       <text x="450.00" y="129.53" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="39.75px" lengthAdjust="spacingAndGlyphs">-12.20</text>
#>       <text x="450.00" y="152.80" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="107.27px" lengthAdjust="spacingAndGlyphs">(-13.22 to -11.18)</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="362.38,179.52 537.62,179.52 537.62,20.91 362.38,20.91 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzYyLjM4fDUzNy42MnwxODQuMjh8MzQyLjg5">
#>         <rect x="362.38" y="184.28" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwxODQuMjh8MzQyLjg5)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMzYwLjAwfDU0MC4wMHwxODIuMzh8MzQ1Ljc0">
#>         <rect x="360.00" y="182.38" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYwLjAwfDU0MC4wMHwxODIuMzh8MzQ1Ljc0)">
#> </g>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwxODQuMjh8MzQyLjg5)">
#>       <rect x="362.38" y="184.28" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="450.00" y="219.46" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="31.95px" lengthAdjust="spacingAndGlyphs">-0.54</text>
#>       <text x="450.00" y="242.74" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="88.05px" lengthAdjust="spacingAndGlyphs">(-1.80 to 0.71)</text>
#>       <text x="450.00" y="292.89" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="19.50px" lengthAdjust="spacingAndGlyphs">NA</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="362.38,342.89 537.62,342.89 537.62,184.28 362.38,184.28 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzYyLjM4fDUzNy42MnwzNDcuNjR8NTA2LjI2">
#>         <rect x="362.38" y="347.64" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwzNDcuNjR8NTA2LjI2)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMzYwLjAwfDU0MC4wMHwzNDUuNzR8NTA5LjEx">
#>         <rect x="360.00" y="345.74" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYwLjAwfDU0MC4wMHwzNDUuNzR8NTA5LjEx)">
#> </g>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42MnwzNDcuNjR8NTA2LjI2)">
#>       <rect x="362.38" y="347.64" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <rect x="362.38" y="347.64" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="450.00" y="382.55" text-anchor="middle" style="font-size: 13.24px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="58.80px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <text x="454.87" y="433.66" text-anchor="middle" style="font-size: 11.92px; font-family: &quot;Liberation Sans&quot;;" textLength="48.00px" lengthAdjust="spacingAndGlyphs">Rank = 3</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="362.38,506.26 537.62,506.26 537.62,347.64 362.38,347.64 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzYyLjM4fDUzNy42Mnw1MTEuMDF8NjY5LjYz">
#>         <rect x="362.38" y="511.01" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42Mnw1MTEuMDF8NjY5LjYz)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMzYwLjAwfDU0MC4wMHw1MDkuMTF8NjcyLjQ4">
#>         <rect x="360.00" y="509.11" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYwLjAwfDU0MC4wMHw1MDkuMTF8NjcyLjQ4)">
#> </g>
#>     <g clip-path="url(#cpMzYyLjM4fDUzNy42Mnw1MTEuMDF8NjY5LjYz)">
#>       <rect x="362.38" y="511.01" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <polyline points="498.68,663.75 498.68,516.89 " style="stroke-width: 0.75; stroke: #CCCCCC;"/>
#>       <polyline points="471.02,565.84 526.34,565.84 " style="stroke-width: 1.12; stroke-dasharray: 1.50,4.50;"/>
#>       <polyline points="471.02,570.74 471.02,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="526.34,570.74 526.34,560.95 " style="stroke-width: 1.12;"/>
#>       <polyline points="488.53,565.84 508.83,565.84 " style="stroke-width: 1.12;"/>
#>       <polyline points="488.53,575.63 488.53,556.05 " style="stroke-width: 1.12;"/>
#>       <polyline points="508.83,575.63 508.83,556.05 " style="stroke-width: 1.12;"/>
#>       <polygon points="496.54,567.98 500.82,567.98 500.82,563.70 496.54,563.70 " style="stroke-width: 0.75; stroke: none; fill: #000000;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="376.98" y1="669.63" x2="498.68" y2="669.63" style="stroke-width: 0.75;"/>
#>       <line x1="376.98" y1="669.63" x2="376.98" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="417.55" y1="669.63" x2="417.55" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="458.11" y1="669.63" x2="458.11" y2="677.56" style="stroke-width: 0.75;"/>
#>       <line x1="498.68" y1="669.63" x2="498.68" y2="677.56" style="stroke-width: 0.75;"/>
#>       <text x="376.98" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-15</text>
#>       <text x="417.55" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="6.86px" lengthAdjust="spacingAndGlyphs">-10</text>
#>       <text x="458.11" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="4.22px" lengthAdjust="spacingAndGlyphs">-5</text>
#>       <text x="498.68" y="683.47" text-anchor="middle" style="font-size: 4.75px; font-family: &quot;Liberation Sans&quot;;" textLength="2.64px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <polygon points="362.38,669.63 537.62,669.63 537.62,511.01 362.38,511.01 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTQyLjM4fDcxNy42MnwyMC45MXwxNzkuNTI=">
#>         <rect x="542.38" y="20.91" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwyMC45MXwxNzkuNTI=)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNTQwLjAwfDcyMC4wMHwxOS4wMXwxODIuMzg=">
#>         <rect x="540.00" y="19.01" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQwLjAwfDcyMC4wMHwxOS4wMXwxODIuMzg=)">
#> </g>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwyMC45MXwxNzkuNTI=)">
#>       <rect x="542.38" y="20.91" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="630.00" y="56.09" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="39.75px" lengthAdjust="spacingAndGlyphs">-12.20</text>
#>       <text x="630.00" y="79.37" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="107.27px" lengthAdjust="spacingAndGlyphs">(-12.93 to -11.47)</text>
#>       <text x="630.00" y="129.53" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="39.75px" lengthAdjust="spacingAndGlyphs">-12.20</text>
#>       <text x="630.00" y="152.80" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="107.27px" lengthAdjust="spacingAndGlyphs">(-12.93 to -11.47)</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="542.38,179.52 717.62,179.52 717.62,20.91 542.38,20.91 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTQyLjM4fDcxNy42MnwxODQuMjh8MzQyLjg5">
#>         <rect x="542.38" y="184.28" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwxODQuMjh8MzQyLjg5)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNTQwLjAwfDcyMC4wMHwxODIuMzh8MzQ1Ljc0">
#>         <rect x="540.00" y="182.38" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQwLjAwfDcyMC4wMHwxODIuMzh8MzQ1Ljc0)">
#> </g>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwxODQuMjh8MzQyLjg5)">
#>       <rect x="542.38" y="184.28" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <text x="630.00" y="219.46" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="31.95px" lengthAdjust="spacingAndGlyphs">-0.54</text>
#>       <text x="630.00" y="242.74" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="88.05px" lengthAdjust="spacingAndGlyphs">(-1.58 to 0.49)</text>
#>       <text x="630.00" y="292.89" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="19.50px" lengthAdjust="spacingAndGlyphs">NA</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="542.38,342.89 717.62,342.89 717.62,184.28 542.38,184.28 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTQyLjM4fDcxNy42MnwzNDcuNjR8NTA2LjI2">
#>         <rect x="542.38" y="347.64" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwzNDcuNjR8NTA2LjI2)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNTQwLjAwfDcyMC4wMHwzNDUuNzR8NTA5LjEx">
#>         <rect x="540.00" y="345.74" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQwLjAwfDcyMC4wMHwzNDUuNzR8NTA5LjEx)">
#> </g>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42MnwzNDcuNjR8NTA2LjI2)">
#>       <rect x="542.38" y="347.64" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="630.00" y="382.83" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="27.28px" lengthAdjust="spacingAndGlyphs">0.00</text>
#>       <text x="630.00" y="406.10" text-anchor="middle" style="font-size: 14.03px; font-family: &quot;Liberation Sans&quot;;" textLength="88.05px" lengthAdjust="spacingAndGlyphs">(-1.25 to 1.25)</text>
#>       <text x="630.00" y="456.26" text-anchor="middle" style="font-size: 14.03px;fill: #8C8C8C; font-family: &quot;Liberation Sans&quot;;" textLength="19.50px" lengthAdjust="spacingAndGlyphs">NA</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="542.38,506.26 717.62,506.26 717.62,347.64 542.38,347.64 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTQyLjM4fDcxNy42Mnw1MTEuMDF8NjY5LjYz">
#>         <rect x="542.38" y="511.01" width="175.25" height="158.62"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42Mnw1MTEuMDF8NjY5LjYz)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNTQwLjAwfDcyMC4wMHw1MDkuMTF8NjcyLjQ4">
#>         <rect x="540.00" y="509.11" width="180.00" height="163.37"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQwLjAwfDcyMC4wMHw1MDkuMTF8NjcyLjQ4)">
#> </g>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42Mnw1MTEuMDF8NjY5LjYz)">
#>       <rect x="542.38" y="511.01" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #F2F2F2;"/>
#>       <rect x="542.38" y="511.01" width="175.25" height="158.62" style="stroke-width: 0.75; fill: #FFFFFF;"/>
#>       <text x="630.00" y="541.37" text-anchor="middle" style="font-size: 13.24px; font-weight: bold; font-family: &quot;Liberation Sans&quot;;" textLength="77.20px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <text x="634.87" y="597.03" text-anchor="middle" style="font-size: 11.92px; font-family: &quot;Liberation Sans&quot;;" textLength="48.00px" lengthAdjust="spacingAndGlyphs">Rank = 4</text>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <polygon points="542.38,669.63 717.62,669.63 717.62,511.01 542.38,511.01 " style="stroke-width: 0.75; stroke: #D9D9D9;"/>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMC4wMHwxODAuMDB8OTQuMTZ8MTgyLjM4">
#>         <rect x="0.00" y="94.16" width="180.00" height="88.22"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHwxODAuMDB8OTQuMTZ8MTgyLjM4)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTQuMjZ8MTc0LjMwfDk0LjE2fDE3MC45Nw==">
#>         <rect x="14.26" y="94.16" width="160.04" height="76.81"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTQuMjZ8MTc0LjMwfDk0LjE2fDE3MC45Nw==)">
#>       <polyline points="20.18,97.00 69.58,168.13 118.97,168.13 168.37,168.13 " style="stroke-width: 1.35;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="20.18" y1="170.97" x2="168.37" y2="170.97" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="20.18" y1="170.97" x2="20.18" y2="174.81" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="69.58" y1="170.97" x2="69.58" y2="174.81" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="118.97" y1="170.97" x2="118.97" y2="174.81" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="168.37" y1="170.97" x2="168.37" y2="174.81" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="20.18" y="180.48" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="168.37" y="180.48" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <line x1="16.16" y1="168.13" x2="16.16" y2="97.00" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="16.16" y1="168.13" x2="11.55" y2="168.13" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="16.16" y1="132.56" x2="11.55" y2="132.56" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="16.16" y1="97.00" x2="11.55" y2="97.00" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="6.04" y="170.03" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="5.12" y="134.47" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="7.69px" lengthAdjust="spacingAndGlyphs">0.5</text>
#>       <text x="6.04" y="98.90" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMTgwLjAwfDM2MC4wMHwyNTcuNTN8MzQ1Ljc0">
#>         <rect x="180.00" y="257.53" width="180.00" height="88.22"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTgwLjAwfDM2MC4wMHwyNTcuNTN8MzQ1Ljc0)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMTk0LjI2fDM1NC4zMHwyNTcuNTN8MzM0LjM0">
#>         <rect x="194.26" y="257.53" width="160.04" height="76.81"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMTk0LjI2fDM1NC4zMHwyNTcuNTN8MzM0LjM0)">
#>       <polyline points="200.18,331.49 249.58,280.50 298.97,315.35 348.37,327.51 " style="stroke-width: 1.35;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="200.18" y1="334.34" x2="348.37" y2="334.34" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="200.18" y1="334.34" x2="200.18" y2="338.18" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="249.58" y1="334.34" x2="249.58" y2="338.18" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="298.97" y1="334.34" x2="298.97" y2="338.18" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="348.37" y1="334.34" x2="348.37" y2="338.18" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="200.18" y="343.84" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="348.37" y="343.84" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <line x1="196.16" y1="331.49" x2="196.16" y2="260.37" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="196.16" y1="331.49" x2="191.55" y2="331.49" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="196.16" y1="295.93" x2="191.55" y2="295.93" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="196.16" y1="260.37" x2="191.55" y2="260.37" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="186.04" y="333.40" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="185.12" y="297.83" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="7.69px" lengthAdjust="spacingAndGlyphs">0.5</text>
#>       <text x="186.04" y="262.27" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpMzYwLjAwfDU0MC4wMHw0MjAuODl8NTA5LjEx">
#>         <rect x="360.00" y="420.89" width="180.00" height="88.22"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzYwLjAwfDU0MC4wMHw0MjAuODl8NTA5LjEx)">
#> </g>
#>     <defs>
#>       <clipPath id="cpMzc0LjI2fDUzNC4zMHw0MjAuODl8NDk3Ljcx">
#>         <rect x="374.26" y="420.89" width="160.04" height="76.81"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMzc0LjI2fDUzNC4zMHw0MjAuODl8NDk3Ljcx)">
#>       <polyline points="380.18,494.86 429.58,481.92 478.97,469.47 528.37,462.07 " style="stroke-width: 1.35;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="380.18" y1="497.71" x2="528.37" y2="497.71" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="380.18" y1="497.71" x2="380.18" y2="501.55" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="429.58" y1="497.71" x2="429.58" y2="501.55" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="478.97" y1="497.71" x2="478.97" y2="501.55" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="528.37" y1="497.71" x2="528.37" y2="501.55" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="380.18" y="507.21" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="528.37" y="507.21" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <line x1="376.16" y1="494.86" x2="376.16" y2="423.74" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="376.16" y1="494.86" x2="371.55" y2="494.86" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="376.16" y1="459.30" x2="371.55" y2="459.30" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="376.16" y1="423.74" x2="371.55" y2="423.74" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="366.04" y="496.76" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="365.12" y="461.20" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="7.69px" lengthAdjust="spacingAndGlyphs">0.5</text>
#>       <text x="366.04" y="425.64" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTQwLjAwfDcyMC4wMHw1ODQuMjZ8NjcyLjQ4">
#>         <rect x="540.00" y="584.26" width="180.00" height="88.22"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTQwLjAwfDcyMC4wMHw1ODQuMjZ8NjcyLjQ4)">
#> </g>
#>     <defs>
#>       <clipPath id="cpNTU0LjI2fDcxNC4zMHw1ODQuMjZ8NjYxLjA4">
#>         <rect x="554.26" y="584.26" width="160.04" height="76.81"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTU0LjI2fDcxNC4zMHw1ODQuMjZ8NjYxLjA4)">
#>       <polyline points="560.18,658.23 609.58,651.05 658.97,628.64 708.37,623.88 " style="stroke-width: 1.35;"/>
#>     </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <line x1="560.18" y1="661.08" x2="708.37" y2="661.08" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="560.18" y1="661.08" x2="560.18" y2="664.92" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="609.58" y1="661.08" x2="609.58" y2="664.92" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="658.97" y1="661.08" x2="658.97" y2="664.92" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="708.37" y1="661.08" x2="708.37" y2="664.92" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="560.18" y="670.58" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="708.37" y="670.58" text-anchor="middle" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">4</text>
#>       <line x1="556.16" y1="658.23" x2="556.16" y2="587.11" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="556.16" y1="658.23" x2="551.55" y2="658.23" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="556.16" y1="622.67" x2="551.55" y2="622.67" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <line x1="556.16" y1="587.11" x2="551.55" y2="587.11" style="stroke-width: 0.60; stroke: #B3B3B3;"/>
#>       <text x="546.04" y="660.13" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">0</text>
#>       <text x="545.12" y="624.57" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="7.69px" lengthAdjust="spacingAndGlyphs">0.5</text>
#>       <text x="546.04" y="589.01" style="font-size: 5.54px;fill: #B3B3B3; font-family: &quot;Liberation Sans&quot;;" textLength="3.08px" lengthAdjust="spacingAndGlyphs">1</text>
#>     </g>
#>     <g clip-path="url(#cpNTQyLjM4fDcxNy42Mnw1MTEuMDF8NjY5LjYz)">
#> </g>
#>     <g clip-path="url(#cpMC4wMHw3MjAuMDB8MC4wMHw3MjAuMDA=)">
#>       <text x="360.00" y="694.34" text-anchor="middle" style="font-size: 9.00px; font-family: &quot;Liberation Sans&quot;;" textLength="284.34px" lengthAdjust="spacingAndGlyphs">Mean Difference with 95% confidence interval &amp; 95% prediction interval</text>
#>       <text x="0.00" y="708.60" style="font-size: 9.00px; font-family: &quot;Liberation Sans&quot;;" textLength="413.33px" lengthAdjust="spacingAndGlyphs">Key: NMA results in black; Pairwise MA results in grey. 95% confidence interval presented as error bars.</text>
#>       <text x="0.00" y="718.10" style="font-size: 9.00px; font-family: &quot;Liberation Sans&quot;;" textLength="214.00px" lengthAdjust="spacingAndGlyphs">Interventions are ranked and sorted by SUCRA value.</text>
#>     </g>
#>     <defs>
#>       <clipPath id="cpNTkuMDR8Njg5Ljc2fDc4LjA1fDU5OS4wNA==">
#>         <rect x="59.04" y="78.05" width="630.72" height="520.99"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpNTkuMDR8Njg5Ljc2fDc4LjA1fDU5OS4wNA==)">
#> </g>
#>   </g>
#> </svg>
#> 
```
