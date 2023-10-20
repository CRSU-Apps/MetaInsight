#' Create the meta-regression page.
#'
#' @param id ID of the module
#' @return Div containing the meta-regression page
meta_regression_tab_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      div(
        h2("Meta-Regression"),
        style = "display: inline-block;"
      ),
      div(
        style = "display: inline-block; padding-right: 20pt;"
      ),
      div(
        h3(textOutput(outputId = ns("subtitle"))),
        style = "display: inline-block;"
      )
    )
  )
}

#' Create the meta-regression server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
meta_regression_tab_server <- function(id, all_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    covariate_title <- shiny::reactive({
      FindCovariateNames(all_data())[1]
    })
    
    covariate_name <- shiny::reactive({
      GetFriendlyCovariateName(covariate_title())
    })
    
    output$subtitle <- shiny::renderText({
      return(glue::glue("Covariate: {covariate_name()}"))
    })
  })
}
