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
          # Add a new module here for the summary panel
        ),
        tabPanel(
          title = "4b. Baseline Risk Analysis",
          outcome_dependent_ui(
            id = ns("baseline_risk_analysis_outer"),
            inner_ui_expression = {
              baseline_risk_analysis_panel_ui(id = ns("baseline_risk_analysis"))
            }
          )
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
            outcome_dependent_ui(
              id = ns("covariate_analysis_outer"),
              inner_ui_expression = {
                covariate_analysis_panel_ui(id = ns("covariate_analysis"))
              }
            )
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
#' @param outcome_measure Reactive containing the outcome measure of the analysis.
meta_regression_tab_server <- function(id, all_data, outcome_measure) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$has_covariates <- reactive({
      length(FindCovariateNames(all_data())) > 0
    })
    shiny::outputOptions(x = output, name = "has_covariates", suspendWhenHidden = FALSE)
    
    # Baseline risk analysis
    outcome_dependent_server(
      id = "baseline_risk_analysis_outer",
      outcome_measure = outcome_measure,
      supported_measures = c("MD", "OR"),
      inner_server_expression = {
        baseline_risk_analysis_panel_server(
          id = "baseline_risk_analysis"
        )
      }
    )
    
    # Covariate analysis
    outcome_dependent_server(
      id = "covariate_analysis_outer",
      outcome_measure = outcome_measure,
      supported_measures = c("MD", "OR", "RR"),
      inner_server_expression = {
        covariate_analysis_panel_server(
          id = "covariate_analysis",
          all_data = all_data
        )
      }
    )
  })
}
