CreateTitle <- function(index) {
  return(paste0(ifelse(is.null(index), "", paste0(index, ". ")), "Meta-regression"))
}

MetaRegressionTabUi <- function(id, title) {
  ns <- NS(id)
  shiny::tabPanel(
    title = title,
    shiny::selectInput(
      inputId = ns("covariate_box"),
      label = "Select covariate",
      choices = c()
    ),
    style = "class: 'main'"
  )
}

MetaRegressionTabServer <- function(id, all_data, sub_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # shiny::updateSelectInput(inputId = "covariate_box", choices = FindCovariateNames(all_data))
    covariate = shiny::reactive({ input$covariate_box })
  })
}
