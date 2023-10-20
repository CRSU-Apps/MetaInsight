CreateTitle <- function(index) {
  return(paste0(ifelse(is.null(index), "", paste0(index, ". ")), "Meta-regression"))
}

MetaRegressionTabUi <- function(id, title) {
  ns <- NS(id)
  shiny::tabPanel(
    title = title
  )
}

MetaRegressionTabServer <- function(id, all_data) {
  shiny::moduleServer(id, function(input, output, session) {
    covariate = shiny::reactive({
      FindCovariateNames(all_data())[1]
    })
  })
}
