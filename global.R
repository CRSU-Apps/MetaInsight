###### MetaInsight ######

# The version of BUGSnet on the CRSU GitHub account needs to be used 
# remotes::install_github("https://github.com/CRSU-Apps/BUGSnet")
# You may wish to remove this version of BUGSnet afterwards using
# remove.packages("BUGSnet")

# plyr is recommended to be loaded before dplyr for better performance
library(plyr)

library(bnma)
library(BUGSnet)
library(combinat)
library(cookies)
library(cowplot)
library(data.table)
library(dplyr)
library(knitr)
library(gemtc)
library(ggiraphExtra)
library(ggplot2)
library(ggrepel)
library(glue)
library(magick)
library(metafor)
library(netmeta)
library(patchwork)
library(plotly)
library(plotrix)
library(R.utils)
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

R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)
