## R CMD check results

0 errors | 0 warnings | 1 note

NOTE

- We are using `coda:::gelman.preplot()` because the original plot provided by 
this package is a base R plot and we wished to use ggplot2 so are accessing 
the underlying data instead. We have checked that the package is stable 
and therefore consider it unlikely for this function to be removed. 

INFO

- We are aware that we have many dependencies and have removed as many as possible: 
https://github.com/CRSU-Apps/MetaInsight/issues/245 Given that the main use of the
package is through the shiny app, it is not ideal to move them to Suggests as
this would make some parts of the app non-functional.
