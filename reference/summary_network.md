# Produce a plot of the network using `netmeta::netgraph()`

Produce a plot of the network using
[`netmeta::netgraph()`](https://rdrr.io/pkg/netmeta/man/netgraph.html)

## Usage

``` r
summary_network(
  configured_data,
  style,
  label_size = 1,
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

- style:

  character. The plot to produce, either `netgraph` or `netplot`

- label_size:

  numeric. The size of labels in the plots. Default of `1`.

- title:

  character. Title of plot. Default of no title.

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

summary_network(configured_data = configured_data,
                style = "netgraph")
#> <?xml version="1.0" encoding="UTF-8"?>
#> <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="122 55 340 238">
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
#>     <rect width="540.00" height="360.00" style="stroke: none; fill: #FFFFFF;"/>
#>     <defs>
#>       <clipPath id="cpMC4wMHw1NDAuMDB8MC4wMHwzNjAuMDA=">
#>         <rect x="0.00" y="0.00" width="540.00" height="360.00"/>
#>       </clipPath>
#>     </defs>
#>     <g clip-path="url(#cpMC4wMHw1NDAuMDB8MC4wMHwzNjAuMDA=)">
#>       <polyline points="284.40,77.04 188.64,172.80 " style="stroke-width: 6.00; stroke: #9E9E9E;"/>
#>       <polyline points="188.64,172.80 284.40,268.56 " style="stroke-width: 4.24; stroke: #9E9E9E;"/>
#>       <polyline points="188.64,172.80 380.16,172.80 " style="stroke-width: 6.00; stroke: #9E9E9E;"/>
#>       <circle cx="284.40" cy="77.04" r="3.60" style="stroke-width: 0.75; fill: #000000;"/>
#>       <circle cx="188.64" cy="172.80" r="3.60" style="stroke-width: 0.75; fill: #000000;"/>
#>       <circle cx="284.40" cy="268.56" r="3.60" style="stroke-width: 0.75; fill: #000000;"/>
#>       <circle cx="380.16" cy="172.80" r="3.60" style="stroke-width: 0.75; fill: #000000;"/>
#>       <text x="288.09" y="73.36" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="64.69px" lengthAdjust="spacingAndGlyphs">the_Butcher</text>
#>       <text x="184.96" y="169.11" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="53.34px" lengthAdjust="spacingAndGlyphs">the_Great</text>
#>       <text x="280.71" y="280.50" text-anchor="end" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="48.69px" lengthAdjust="spacingAndGlyphs">the_Little</text>
#>       <text x="383.84" y="184.75" style="font-size: 12.00px; font-family: &quot;Liberation Sans&quot;;" textLength="67.61px" lengthAdjust="spacingAndGlyphs">the_Younger</text>
#>       <text x="230.38" y="135.23" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.82" y="134.99" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.25" y="135.23" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.02" y="135.81" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.25" y="136.40" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.82" y="136.64" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="230.38" y="136.40" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="230.62" y="135.81" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="229.82" y="135.81" text-anchor="middle" style="font-size: 12.00px;fill: #228B22; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="243.79" y="230.93" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="243.22" y="230.69" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="242.66" y="230.93" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="242.42" y="231.51" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="242.66" y="232.09" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="243.22" y="232.34" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="243.79" y="232.09" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="244.02" y="231.51" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="243.22" y="231.51" text-anchor="middle" style="font-size: 12.00px;fill: #228B22; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">1</text>
#>       <text x="298.37" y="176.41" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.81" y="176.16" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.24" y="176.41" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.01" y="176.99" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.24" y="177.57" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.81" y="177.82" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="298.37" y="177.57" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="298.61" y="176.99" text-anchor="middle" style="font-size: 12.00px;fill: #FFFFFF; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>       <text x="297.81" y="176.99" text-anchor="middle" style="font-size: 12.00px;fill: #228B22; font-family: &quot;Liberation Sans&quot;;" textLength="6.67px" lengthAdjust="spacingAndGlyphs">2</text>
#>     </g>
#>   </g>
#> </svg>
#> 
```
