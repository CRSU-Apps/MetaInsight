#' Create the meta-regression panel.
#'
#' @param id ID of the module
#' @return Div containing the module
meta_regression_tab_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      tabsetPanel(
        id = "regression_tabs",
        tabPanel(
          title = "4a. Summary",
          metaregression_summary_panel_ui(id = ns("metaregression_summary_panel"))
        ),
        tabPanel(
          title = "4b. Baseline Risk Analysis"
          # Add a new module here for the baseline risk analysis panel
        ),
        tabPanel(
          title = "4c. Covariate Analysis",
          conditionalPanel(
            condition = "!output.has_covariates",
            ns = ns,
            div(
              h3(
                "No covariate data. To add covariate data, add a column titled",
                code("covar.*"),
                "where the",
                code("*"),
                "is replaced by the covariate name. eg. ",
                code("covar.age")
              ),
              style = "color: red; font-style: italic; font-weight: bold;"
            )
          ),
          conditionalPanel(
            condition = "output.has_covariates",
            ns = ns,
            covariate_analysis_panel_ui(id = ns("covariate_analysis"))
          )
        )
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
    
    metaregression_summary_panel_server(id = "metaregression_summary_panel", all_data = all_data)
  
    output$has_covariates <- reactive({
      length(FindCovariateNames(all_data())) > 0
    })
    shiny::outputOptions(x = output, name = "has_covariates", suspendWhenHidden = FALSE)
    
    covariate_analysis_panel_server(id = "covariate_analysis", all_data = all_data)
  })
}
