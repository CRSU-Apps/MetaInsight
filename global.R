###### MetaInsight ######

LoadOrInstall <- function(package_name, install_function = install.packages) {
  print(paste0("*** - *** Loading package: '", package_name, "' *** - ***"))
  tryCatch(
    {
      library(package_name, character.only = TRUE)
    },
    error = function(exptn) {
      print(paste0("*** - *** Installing package: '", package_name, "' *** - ***"))
      if (class(exptn)[1] == "packageNotFoundError") {
        install_function(package_name)
        library(package_name, character.only = TRUE)
      } else {
        stop(exptn)
      }
    }
  )
}

InstallBugsNet <- function(ignored) {
  LoadOrInstall("remotes")
  remotes::install_github("audrey-b/BUGSnet")
}

# plyr is recommended to be loaded before dplyr for better performance
LoadOrInstall("plyr")

LoadOrInstall("bnma")
LoadOrInstall("BUGSnet", install_function = InstallBugsNet)
LoadOrInstall("combinat")
LoadOrInstall("cookies")
LoadOrInstall("cowplot")
LoadOrInstall("data.table")
LoadOrInstall("data.tree")
LoadOrInstall("dplyr")
LoadOrInstall("DT")
LoadOrInstall("future")
LoadOrInstall("jsonlite")
LoadOrInstall("knitr")
LoadOrInstall("gemtc")
LoadOrInstall("ggiraphExtra")
LoadOrInstall("ggplot2")
LoadOrInstall("ggpubr")
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
