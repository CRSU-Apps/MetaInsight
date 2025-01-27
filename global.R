###### MetaInsight ######

LoadOrInstall <- function(package_name) {
  tryCatch(
    {
      library(package_name, character.only = TRUE)
    },
    error = function(exptn) {
      if (class(exptn)[1] == "packageNotFoundError") {
        install.packages(package_name)
        library(package_name, character.only = TRUE)
      } else {
        stop(exptn)
      }
    }
  )
}

# plyr is recommended to be loaded before dplyr for better performance
LoadOrInstall("plyr")

LoadOrInstall("bnma")
LoadOrInstall("BUGSnet")
LoadOrInstall("combinat")
LoadOrInstall("cookies")
LoadOrInstall("cowplot")
LoadOrInstall("data.table")
LoadOrInstall("data.tree")
LoadOrInstall("dplyr")
LoadOrInstall("future")
LoadOrInstall("jsonlite")
LoadOrInstall("knitr")
LoadOrInstall("gemtc")
LoadOrInstall("ggiraphExtra")
LoadOrInstall("ggplot2")
LoadOrInstall("ggrepel")
LoadOrInstall("glue")
LoadOrInstall("lubridate")
LoadOrInstall("magick")
LoadOrInstall("matrixcalc")
LoadOrInstall("MCMCvis")
LoadOrInstall("metafor")
LoadOrInstall("mvtnorm")
LoadOrInstall("netmeta")
LoadOrInstall("patchwork")
LoadOrInstall("plotly")
LoadOrInstall("plotrix")
LoadOrInstall("promises")
LoadOrInstall("R.utils")
LoadOrInstall("R6")
LoadOrInstall("rio")
LoadOrInstall("shiny")
LoadOrInstall("shinyalert")
LoadOrInstall("shinyAce")
LoadOrInstall("shinyBS")
LoadOrInstall("shinycssloaders")
LoadOrInstall("shinydashboard")
LoadOrInstall("shinyjs")
LoadOrInstall("shinyWidgets")
LoadOrInstall("stringr")
LoadOrInstall("rmarkdown")
LoadOrInstall("tidyr")

R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)

future::plan(multisession)

options(shiny.sanitize.errors = FALSE)
