###### MetaInsight ######

# plyr is recommended to be loaded before dplyr for better performance
library(plyr)

library(BUGSnet)
library(cowplot)
library(data.table)
library(dplyr)
library(knitr)
library(gemtc)
library(ggiraphExtra)
library(ggplot2)
library(ggrepel)
library(magick)
library(metafor)
library(netmeta)
library(patchwork)
library(plotly)
library(shiny) 
library(shinyalert)
library(shinyAce)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(rmarkdown)
library(tidyr)


load_library <- function(library_name) {
  if(!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
  }
  require(library_name, character.only = TRUE)
}


#Call required library for pie plotting
load_library('plotrix')

#package required for function: sortres.matrix for mtcMatrix
load_library("combinat")
