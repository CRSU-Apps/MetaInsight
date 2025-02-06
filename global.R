###### MetaInsight ######

source("_dependencies.R")

R.utils::sourceDirectory(path = "R", modifiedOnly = FALSE)

future::plan(multisession)

options(shiny.sanitize.errors = FALSE)
