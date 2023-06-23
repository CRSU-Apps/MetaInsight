###### MEtaInsight ######

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
library(plyr)
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


#Call required library for pie plotting
libplotrix <- function(){
  install.packages('plotrix')
  require(plotrix)
} 
ifelse(any(grepl('plotrix',installed.packages()[,1])), require(plotrix), libplotrix())

#package required for function: sortres.matrix for mtcMatrix
libcombinat <- function(){
  install.packages("combinat")
  require(combinat)
}
ifelse(any(grepl('combinat',installed.packages()[,1])), require(combinat), libcombinat())
